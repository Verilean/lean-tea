# syntax=docker/dockerfile:1
# Build context = lean-tea/ directory.
#
# Two stages: build (Debian + elan + Lean toolchain + cc) and runtime
# (Debian slim + just the binary + dist/). Resulting image ~170MB.

# ---------- build stage ----------
FROM debian:bookworm-slim AS build

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl git \
      build-essential \
      libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

# Install elan and the toolchain pinned in lean-toolchain.
ENV PATH="/root/.elan/bin:${PATH}"
RUN curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
    | sh -s -- -y --default-toolchain none

WORKDIR /src
COPY lean-toolchain ./lean-toolchain
RUN elan toolchain install "$(cat lean-toolchain)"

# Copy the rest of the source.
COPY . /src

# Build both the CLI exe and the HTTP server.
RUN lake build canvas_serve

# Pre-generate static SPA assets so the runtime image is self-contained.
RUN ./.lake/build/bin/english --gen /src/dist

# ---------- runtime stage ----------
FROM debian:bookworm-slim AS runtime

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates libgmp10 \
      curl \
    && rm -rf /var/lib/apt/lists/*

# Non-root user for the daemon.
RUN useradd -r -u 1000 -d /app -s /sbin/nologin lean

COPY --from=build /src/.lake/build/bin/canvas_serve /app/canvas_serve
COPY --from=build /src/dist /app/dist

RUN mkdir -p /data && chown -R lean:lean /app /data

ENV DIST_DIR=/app/dist
ENV DB_PATH=/data/english.sqlite
ENV PORT=8080

EXPOSE 8080
VOLUME /data

USER lean
WORKDIR /app
ENTRYPOINT ["/app/canvas_serve"]
