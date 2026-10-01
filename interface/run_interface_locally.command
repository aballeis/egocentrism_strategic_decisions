#!/bin/bash
cd "$(dirname "$0")"
python3 -c "import flask" 2>/dev/null || python3 -m pip install -r requirements.txt
(sleep 2; open http://127.0.0.1:8080/static/local.html) &
python3 app.py
