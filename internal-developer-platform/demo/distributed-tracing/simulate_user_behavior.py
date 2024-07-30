import uuid
import random
import time
from datetime import datetime
from typing import Dict, List, Optional, Any

class DistributedTracer:
    """
    A class to manage distributed tracing across multiple services.
    """

    def __init__(self):
        self.traces: Dict[str, Dict[str, Any]] = {}

    def start_trace(self, service: str, operation: str) -> str:
        """
        Start a new trace.

        Args:
            service (str): The name of the service starting the trace.
            operation (str): The name of the operation being traced.

        Returns:
            str: The unique identifier for the new trace.
        """
        trace_id = str(uuid.uuid4())
        self.traces[trace_id] = {
            "trace_id": trace_id,
            "start_time": datetime.now(),
            "service": service,
            "operation": operation,
            "spans": []
        }
        return trace_id

    def add_span(self, trace_id: str, service: str, operation: str, parent_id: Optional[str] = None) -> str:
        """
        Add a new span to an existing trace.

        Args:
            trace_id (str): The ID of the trace to add the span to.
            service (str): The name of the service creating the span.
            operation (str): The name of the operation being traced.
            parent_id (Optional[str]): The ID of the parent span, if any.

        Returns:
            str: The unique identifier for the new span.
        """
        span_id = str(uuid.uuid4())
        span = {
            "span_id": span_id,
            "parent_id": parent_id,
            "service": service,
            "operation": operation,
            "start_time": datetime.now(),
            "end_time": None
        }
        self.traces[trace_id]["spans"].append(span)
        return span_id

    def end_span(self, trace_id: str, span_id: str) -> None:
        """
        Mark a span as completed.

        Args:
            trace_id (str): The ID of the trace containing the span.
            span_id (str): The ID of the span to end.
        """
        for span in self.traces[trace_id]["spans"]:
            if span["span_id"] == span_id:
                span["end_time"] = datetime.now()
                break

    def get_trace(self, trace_id: str) -> Optional[Dict[str, Any]]:
        """
        Retrieve a trace by its ID.

        Args:
            trace_id (str): The ID of the trace to retrieve.

        Returns:
            Optional[Dict[str, Any]]: The trace data, or None if not found.
        """
        return self.traces.get(trace_id)

class CustomerPortalUI:
    """
    Simulates the customer-facing UI for viewing orders.
    """

    def __init__(self, tracer: DistributedTracer):
        self.tracer = tracer

    def view_completed_order(self, customer_id: str, order_id: str) -> Dict[str, Any]:
        """
        Simulate a customer viewing a completed order.

        Args:
            customer_id (str): The ID of the customer viewing the order.
            order_id (str): The ID of the order being viewed.

        Returns:
            Dict[str, Any]: The compiled order details.
        """
        trace_id = self.tracer.start_trace("CustomerPortalUI", "view_completed_order")
        span_id = self.tracer.add_span(trace_id, "CustomerPortalUI", "request_order_details")

        # Simulate API call to GraphQL
        time.sleep(random.uniform(0.1, 0.3))
        order_details = GraphQLAPI(self.tracer).get_order_details(trace_id, span_id, customer_id, order_id)

        self.tracer.end_span(trace_id, span_id)
        return order_details

class GraphQLAPI:
    """
    Simulates the GraphQL API that coordinates data fetching from various services.
    """

    def __init__(self, tracer: DistributedTracer):
        self.tracer = tracer

    def get_order_details(self, trace_id: str, parent_span_id: str, customer_id: str, order_id: str) -> Dict[str, Any]:
        """
        Fetch and compile order details from various services.

        Args:
            trace_id (str): The ID of the current trace.
            parent_span_id (str): The ID of the parent span.
            customer_id (str): The ID of the customer.
            order_id (str): The ID of the order.

        Returns:
            Dict[str, Any]: The compiled order details.
        """
        span_id = self.tracer.add_span(trace_id, "GraphQLAPI", "get_order_details", parent_span_id)

        # Parallel calls to services
        order_data = OrderService(self.tracer).get_order(trace_id, span_id, order_id)
        product_data = ProductService(self.tracer).get_product_details(trace_id, span_id, order_data["product_ids"])
        customer_data = CustomerService(self.tracer).get_customer_details(trace_id, span_id, customer_id)

        # Simulate data compilation
        time.sleep(random.uniform(0.05, 0.1))

        self.tracer.end_span(trace_id, span_id)
        return {**order_data, "products": product_data, "customer": customer_data}

class OrderService:
    """
    Simulates the Order Service responsible for fetching order data.
    """

    def __init__(self, tracer: DistributedTracer):
        self.tracer = tracer

    def get_order(self, trace_id: str, parent_span_id: str, order_id: str) -> Dict[str, Any]:
        """
        Fetch order data for a given order ID.

        Args:
            trace_id (str): The ID of the current trace.
            parent_span_id (str): The ID of the parent span.
            order_id (str): The ID of the order to fetch.

        Returns:
            Dict[str, Any]: The order data.
        """
        span_id = self.tracer.add_span(trace_id, "OrderService", "get_order", parent_span_id)

        # Simulate database query
        time.sleep(random.uniform(0.1, 0.2))

        self.tracer.end_span(trace_id, span_id)
        return {"order_id": order_id, "status": "completed", "product_ids": [str(uuid.uuid4()) for _ in range(3)]}

class ProductService:
    """
    Simulates the Product Service responsible for fetching product details.
    """

    def __init__(self, tracer: DistributedTracer):
        self.tracer = tracer

    def get_product_details(self, trace_id: str, parent_span_id: str, product_ids: List[str]) -> List[Dict[str, str]]:
        """
        Fetch product details for a list of product IDs.

        Args:
            trace_id (str): The ID of the current trace.
            parent_span_id (str): The ID of the parent span.
            product_ids (List[str]): A list of product IDs to fetch details for.

        Returns:
            List[Dict[str, str]]: A list of product details.
        """
        span_id = self.tracer.add_span(trace_id, "ProductService", "get_product_details", parent_span_id)

        # Simulate database query
        time.sleep(random.uniform(0.15, 0.25))

        self.tracer.end_span(trace_id, span_id)
        return [{"product_id": pid, "name": f"Product {i+1}"} for i, pid in enumerate(product_ids)]

class CustomerService:
    """
    Simulates the Customer Service responsible for fetching customer details.
    """

    def __init__(self, tracer: DistributedTracer):
        self.tracer = tracer

    def get_customer_details(self, trace_id: str, parent_span_id: str, customer_id: str) -> Dict[str, str]:
        """
        Fetch customer details for a given customer ID.

        Args:
            trace_id (str): The ID of the current trace.
            parent_span_id (str): The ID of the parent span.
            customer_id (str): The ID of the customer to fetch details for.

        Returns:
            Dict[str, str]: The customer details.
        """
        span_id = self.tracer.add_span(trace_id, "CustomerService", "get_customer_details", parent_span_id)

        # Simulate database query
        time.sleep(random.uniform(0.05, 0.15))

        self.tracer.end_span(trace_id, span_id)
        return {"customer_id": customer_id, "name": f"Customer {customer_id}"}

def simulate_user_behavior() -> None:
    """
    Simulate a user viewing a completed order and print the results.
    """
    tracer = DistributedTracer()
    ui = CustomerPortalUI(tracer)

    customer_id = str(uuid.uuid4())
    order_id = str(uuid.uuid4())

    print(f"Simulating: Customer {customer_id} viewing completed order {order_id}")
    order_details = ui.view_completed_order(customer_id, order_id)

    print("\nOrder Details:")
    print(order_details)

    print("\nDistributed Trace:")
    trace = tracer.get_trace(list(tracer.traces.keys())[0])
    if trace:
        print(f"Trace ID: {trace['trace_id']}")
        print(f"Root Operation: {trace['operation']}")
        for span in trace['spans']:
            print(f"  Service: {span['service']}, Operation: {span['operation']}")
            print(f"    Span ID: {span['span_id']}")
            print(f"    Parent ID: {span['parent_id']}")
            if span['end_time']:
                print(f"    Duration: {(span['end_time'] - span['start_time']).total_seconds():.3f}s")
            else:
                print("    Duration: Not completed")
    else:
        print("No trace found")

if __name__ == "__main__":
    simulate_user_behavior()