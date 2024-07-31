from flask import Flask, request, jsonify
import logging
from datetime import datetime
import json

app = Flask(__name__)

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@app.route('/api/traces', methods=['POST'])
def receive_traces():
    current_time = datetime.now().strftime("%a %b %d %Y %H:%M:%S GMT%z (%Z)")
    logger.info(f'[{current_time}] "POST /api/traces" "{request.user_agent}"')
    
    try:
        # Get the raw data
        raw_data = request.get_data(as_text=True)
        logger.info(f'Received raw data: {raw_data}')

        # Try to parse the JSON
        data = json.loads(raw_data)
        
        trace_id = data.get('traceId')
        payload = data.get('payload')
        
        if not trace_id or not payload:
            raise ValueError("Missing traceId or payload")
        
        logger.info(f'Received trace data:')
        logger.info(f'  TraceId: {trace_id}')
        logger.info(f'  Payload: {json.dumps(payload, indent=2)}')
        
        return jsonify({"status": "success"}), 200
    except json.JSONDecodeError as e:
        logger.error(f'JSON Decode Error: {str(e)}')
        return jsonify({"status": "error", "message": f"Invalid JSON: {str(e)}"}), 400
    except Exception as e:
        logger.error(f'Error processing trace data: {str(e)}')
        return jsonify({"status": "error", "message": str(e)}), 400

@app.route('/health', methods=['GET'])
def health_check():
    current_time = datetime.now().strftime("%a %b %d %Y %H:%M:%S GMT%z (%Z)")
    logger.info(f'[{current_time}] "GET /health" "{request.user_agent}"')
    return jsonify({"status": "ok"}), 200

if __name__ == '__main__':
    logger.info("Starting mock Jaeger server on http://localhost:14268")
    app.run(host='localhost', port=14268, debug=True)
