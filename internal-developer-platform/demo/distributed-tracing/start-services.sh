#!/bin/bash

# Start Elasticsearch
/usr/share/elasticsearch/bin/elasticsearch -d

# Start Kibana
/usr/share/kibana/bin/kibana &

# Start Logstash
/usr/share/logstash/bin/logstash -f /etc/logstash/conf.d/logstash.conf &

# Start Prometheus
/prometheus/prometheus --config.file=/prometheus/prometheus.yml &

# Start Grafana
/usr/sbin/grafana-server --config=/etc/grafana/grafana.ini &

# Start Jaeger
/jaeger/jaeger-all-in-one &

# Keep container running
tail -f /dev/null
