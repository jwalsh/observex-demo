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
        # Try to parse the incoming data as JSON
        data = request.get_json(force=True)
        logger.info(f'Received trace data: {json.dumps(data, indent=2)}')
        return jsonify({"status": "success"}), 200
    except json.JSONDecodeError as e:
        # If JSON parsing fails, log the raw data
        logger.error(f'Failed to parse JSON: {str(e)}')
        logger.error(f'Raw data received: {request.data.decode()}')
        return jsonify({"status": "error", "message": "Invalid JSON data"}), 400

@app.route('/health', methods=['GET'])
def health_check():
    current_time = datetime.now().strftime("%a %b %d %Y %H:%M:%S GMT%z (%Z)")
    logger.info(f'[{current_time}] "GET /health" "{request.user_agent}"')
    return jsonify({"status": "ok"}), 200

if __name__ == '__main__':
    logger.info("Starting mock Jaeger server on http://localhost:14268")
    app.run(host='localhost', port=14268, debug=True)
