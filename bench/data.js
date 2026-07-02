window.BENCHMARK_DATA = {
  "lastUpdate": 1782966807418,
  "repoUrl": "https://github.com/Verilean/lean-tea",
  "entries": {
    "HTTP throughput (lean-tea reactor vs nginx)": [
      {
        "commit": {
          "author": {
            "email": "junjihashimoto@users.noreply.github.com",
            "name": "junji hashimoto",
            "username": "junjihashimoto"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "343a3121239c62969b918942a959e60a8eb75ffa",
          "message": "Merge pull request #7 from Verilean/docs/typed-rpc-workflow\n\ndocs: typed RPC workflow in docs/ (delete TUTORIAL.md); bench → gh-pages",
          "timestamp": "2026-07-02T13:05:56+09:00",
          "tree_id": "04ed16c076a87bb706879ef6ccd7e148d91f02d8",
          "url": "https://github.com/Verilean/lean-tea/commit/343a3121239c62969b918942a959e60a8eb75ffa"
        },
        "date": 1782965843122,
        "tool": "customBiggerIsBetter",
        "benches": [
          {
            "name": "lean-tea reactor /health",
            "value": 51935,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /json",
            "value": 54687,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /echo",
            "value": 39168,
            "unit": "RPS"
          },
          {
            "name": "nginx /health",
            "value": 105673,
            "unit": "RPS"
          },
          {
            "name": "nginx /json",
            "value": 99809,
            "unit": "RPS"
          },
          {
            "name": "lean-tea/nginx /health %",
            "value": 49,
            "unit": "%"
          },
          {
            "name": "lean-tea/nginx /json %",
            "value": 55,
            "unit": "%"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "junjihashimoto@users.noreply.github.com",
            "name": "junji hashimoto",
            "username": "junjihashimoto"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f8742e446de463bfc51e6877746e2edb3b9734d6",
          "message": "Merge pull request #8 from Verilean/docs/typed-rpc-workflow\n\nbench: custom chart grouped by endpoint (lean-tea vs nginx on one graph)",
          "timestamp": "2026-07-02T13:30:57+09:00",
          "tree_id": "317794970befe1368515b32a28cc7e773063b801",
          "url": "https://github.com/Verilean/lean-tea/commit/f8742e446de463bfc51e6877746e2edb3b9734d6"
        },
        "date": 1782966806987,
        "tool": "customBiggerIsBetter",
        "benches": [
          {
            "name": "lean-tea reactor /health",
            "value": 50886,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /json",
            "value": 56433,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /echo",
            "value": 39711,
            "unit": "RPS"
          },
          {
            "name": "nginx /health",
            "value": 117994,
            "unit": "RPS"
          },
          {
            "name": "nginx /json",
            "value": 104402,
            "unit": "RPS"
          },
          {
            "name": "lean-tea/nginx /health %",
            "value": 43,
            "unit": "%"
          },
          {
            "name": "lean-tea/nginx /json %",
            "value": 54,
            "unit": "%"
          }
        ]
      }
    ]
  }
}