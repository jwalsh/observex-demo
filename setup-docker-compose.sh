#!/bin/bash

# Define the teams and their services
TEAMS=(
  "customer-experience:customer-portal,customer-service"
  "order-fulfillment:order-service"
  "product-catalog:catalog-service,product-service"
)

# Create a docker-compose.yml file for each team
for TEAM in "${TEAMS[@]}"; do
  TEAM_NAME=${TEAM%%:*}
  SERVICES=${TEAM#*:}

  # Create the docker-compose.yml file
  echo "Creating docker-compose.yml file for $TEAM_NAME..."
  cat > "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml" <<EOF
version: '3'
services:
EOF

  # Add each service to the docker-compose.yml file
  for SERVICE in ${SERVICES//,/ }; do
    echo "  $SERVICE:" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
    echo "    build: ." >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
    echo "    ports:" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
    echo "      - \"8080:8080\"" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
    echo "    depends_on:" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
    echo "      - $TEAM_NAME-db" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
  done

  # Add the database service to the docker-compose.yml file
  echo "  $TEAM_NAME-db:" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
  echo "    image: postgres" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
  echo "    environment:" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
  echo "      - POSTGRES_USER=$TEAM_NAME" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
  echo "      - POSTGRES_PASSWORD=$TEAM_NAME" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
  echo "      - POSTGRES_DB=$TEAM_NAME" >> "internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml"
done

# Create a top-level docker-compose.yml file that includes all teams
echo "Creating top-level docker-compose.yml file..."
cat > "docker-compose.yml" <<EOF
version: '3'
services:
EOF

for TEAM in "${TEAMS[@]}"; do
  TEAM_NAME=${TEAM%%:*}
  echo "  $TEAM_NAME:" >> "docker-compose.yml"
  echo "    extends:" >> "docker-compose.yml"
  echo "      file: internal-developer-platform/teams/$TEAM_NAME/docker-compose.yml" >> "docker-compose.yml"
  echo "      service: $TEAM_NAME" >> "docker-compose.yml"
done