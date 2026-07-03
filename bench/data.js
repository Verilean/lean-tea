window.BENCHMARK_DATA = {
  "lastUpdate": 1783049210657,
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
    ],
    "HTTP throughput (lean-tea backends vs nginx)": [
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
          "id": "025b176533899e9d16bd8212da62658b8564b0c9",
          "message": "Merge pull request #9 from Verilean/docs/typed-rpc-workflow\n\nDocs/typed rpc workflow",
          "timestamp": "2026-07-02T14:25:37+09:00",
          "tree_id": "6987ad3d2ff4c7510714ec385fc9d549e53a3f5a",
          "url": "https://github.com/Verilean/lean-tea/commit/025b176533899e9d16bd8212da62658b8564b0c9"
        },
        "date": 1782970124475,
        "tool": "customBiggerIsBetter",
        "benches": [
          {
            "name": "lean-tea libuv /health",
            "value": 2939,
            "unit": "RPS"
          },
          {
            "name": "lean-tea libuv /json",
            "value": 2925,
            "unit": "RPS"
          },
          {
            "name": "lean-tea libuv /echo",
            "value": 2822,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /health",
            "value": 85392,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /json",
            "value": 88099,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /echo",
            "value": 70693,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /health",
            "value": 105966,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /json",
            "value": 107321,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /echo",
            "value": 83809,
            "unit": "RPS"
          },
          {
            "name": "nginx /health",
            "value": 113132,
            "unit": "RPS"
          },
          {
            "name": "nginx /json",
            "value": 104302,
            "unit": "RPS"
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
          "id": "ee8661150c8a81ca9cdb6c4f11060af1ce5c2859",
          "message": "Merge pull request #10 from Verilean/fix/bench-chart-suite\n\nfix(bench): chart was showing a stale suite — multi-loop reactor was hidden",
          "timestamp": "2026-07-02T15:04:27+09:00",
          "tree_id": "15151d3d6359dc471d56c4f6576f5950d8cbc972",
          "url": "https://github.com/Verilean/lean-tea/commit/ee8661150c8a81ca9cdb6c4f11060af1ce5c2859"
        },
        "date": 1782972463560,
        "tool": "customBiggerIsBetter",
        "benches": [
          {
            "name": "lean-tea libuv /health",
            "value": 2820,
            "unit": "RPS"
          },
          {
            "name": "lean-tea libuv /json",
            "value": 2707,
            "unit": "RPS"
          },
          {
            "name": "lean-tea libuv /echo",
            "value": 2809,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /health",
            "value": 88948,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /json",
            "value": 90237,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /echo",
            "value": 72227,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /health",
            "value": 107828,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /json",
            "value": 107731,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /echo",
            "value": 85368,
            "unit": "RPS"
          },
          {
            "name": "nginx /health",
            "value": 121377,
            "unit": "RPS"
          },
          {
            "name": "nginx /json",
            "value": 105848,
            "unit": "RPS"
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
          "id": "7051e7ac4bf9f1dc073086e3f4f5f1513e154717",
          "message": "Merge pull request #11 from Verilean/feat/leanjs-do-for-loops\n\nLeanJs: do-blocks with for / while / let mut / reassignment",
          "timestamp": "2026-07-03T12:23:41+09:00",
          "tree_id": "684cb2bcfc53d9c846ca472c96f9aeb31c66f13c",
          "url": "https://github.com/Verilean/lean-tea/commit/7051e7ac4bf9f1dc073086e3f4f5f1513e154717"
        },
        "date": 1783049210093,
        "tool": "customBiggerIsBetter",
        "benches": [
          {
            "name": "lean-tea libuv /health",
            "value": 2671,
            "unit": "RPS"
          },
          {
            "name": "lean-tea libuv /json",
            "value": 2866,
            "unit": "RPS"
          },
          {
            "name": "lean-tea libuv /echo",
            "value": 2763,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /health",
            "value": 92984,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /json",
            "value": 94507,
            "unit": "RPS"
          },
          {
            "name": "lean-tea fast /echo",
            "value": 75923,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /health",
            "value": 112351,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /json",
            "value": 112913,
            "unit": "RPS"
          },
          {
            "name": "lean-tea reactor /echo",
            "value": 89288,
            "unit": "RPS"
          },
          {
            "name": "nginx /health",
            "value": 110314,
            "unit": "RPS"
          },
          {
            "name": "nginx /json",
            "value": 114949,
            "unit": "RPS"
          }
        ]
      }
    ]
  }
}