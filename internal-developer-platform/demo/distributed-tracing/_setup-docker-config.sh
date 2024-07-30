#!/bin/bash

# Create prometheus.yml
cat << EOF > prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']

  - job_name: 'node'
    static_configs:
      - targets: ['localhost:9100']
EOF

echo "Created prometheus.yml"

# Create logstash.conf
cat << EOF > logstash.conf
input {
  beats {
    port => 5044
  }
}

filter {
  grok {
    match => { "message" => "%{COMBINEDAPACHELOG}" }
  }
  date {
    match => [ "timestamp" , "dd/MMM/yyyy:HH:mm:ss Z" ]
  }
}

output {
  elasticsearch {
    hosts => ["http://localhost:9200"]
    index => "%{[@metadata][beat]}-%{[@metadata][version]}-%{+YYYY.MM.dd}"
  }
}
EOF

echo "Created logstash.conf"

# Create elasticsearch.yml
cat << EOF > elasticsearch.yml
cluster.name: monitoring-cluster
node.name: node-1
network.host: 0.0.0.0
http.port: 9200
discovery.type: single-node
EOF

echo "Created elasticsearch.yml"

# Create kibana.yml
cat << EOF > kibana.yml
server.port: 5601
server.host: "0.0.0.0"
elasticsearch.hosts: ["http://localhost:9200"]
EOF

echo "Created kibana.yml"

# Create start-services.sh
cat << EOF > start-services.sh
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
EOF

chmod +x start-services.sh
echo "Created start-services.sh"

echo "Demo setup complete. All configuration files have been created."