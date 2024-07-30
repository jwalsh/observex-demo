# Use Ubuntu as the base image
FROM ubuntu:20.04

# Avoid prompts from apt
ENV DEBIAN_FRONTEND=noninteractive

# Install necessary tools
RUN apt-get update && apt-get install -y \
    wget \
    curl \
    gnupg \
    software-properties-common \
    openjdk-11-jdk

# Install Prometheus
RUN wget https://github.com/prometheus/prometheus/releases/download/v2.30.3/prometheus-2.30.3.linux-amd64.tar.gz \
    && tar xvfz prometheus-*.tar.gz \
    && mv prometheus-* prometheus \
    && rm prometheus-*.tar.gz

# Install Grafana
RUN wget -q -O - https://packages.grafana.com/gpg.key | apt-key add - \
    && add-apt-repository "deb https://packages.grafana.com/oss/deb stable main" \
    && apt-get update && apt-get install -y grafana

# Install Elasticsearch
RUN wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | apt-key add - \
    && echo "deb https://artifacts.elastic.co/packages/7.x/apt stable main" | tee /etc/apt/sources.list.d/elastic-7.x.list \
    && apt-get update && apt-get install -y elasticsearch

# Install Logstash
RUN apt-get install -y logstash

# Install Kibana
RUN apt-get install -y kibana

# Install Jaeger
RUN wget https://github.com/jaegertracing/jaeger/releases/download/v1.25.0/jaeger-1.25.0-linux-amd64.tar.gz \
    && tar xvzf jaeger-1.25.0-linux-amd64.tar.gz \
    && mv jaeger-1.25.0-linux-amd64 jaeger \
    && rm jaeger-1.25.0-linux-amd64.tar.gz

# Copy configuration files
COPY prometheus.yml /prometheus/
COPY logstash.conf /etc/logstash/conf.d/
COPY elasticsearch.yml /etc/elasticsearch/
COPY kibana.yml /etc/kibana/

# Expose ports
EXPOSE 9090 3000 9200 5601 16686

# Set up startup script
COPY start-services.sh /
RUN chmod +x /start-services.sh

# Start all services
CMD ["/start-services.sh"]