#!/usr/bin/env python3
import sys
import json

# Define all possible metrics
USER_DATA_METRICS = ["Total No. Commits", "LOC", "LOC per commit", "Commits per day"]

def normalize_metric(values, alpha=0.1):
    """Normalize a list of values to 0-1 with smoothing to avoid exact 0 or 1."""
    min_val = min(values)
    max_val = max(values)
    range_val = max_val - min_val if max_val != min_val else 1
    normalized = [(v - min_val) / range_val for v in values]
    smoothed = [alpha + (1 - 2 * alpha) * n for n in normalized]
    return smoothed

def scale_users(users, config):
    selected_metrics = config.get("metrics", [])
    method = config.get("method", "Percentiles")
    results = []

    # Map metrics to their indices
    metric_indices = [USER_DATA_METRICS.index(m) for m in selected_metrics if m in USER_DATA_METRICS]

    if not metric_indices:
        return [{"name": name, "score": 0.0} for name, _ in users]

    # Extract and normalize + smooth each metric
    metrics_values = []
    for idx in metric_indices:
        metric_vals = [user[1][idx] for user in users]
        metrics_values.append(normalize_metric(metric_vals, alpha=0.1))

    # Compute final scores per user
    for i, (name, _) in enumerate(users):
        user_scores = [metrics_values[m][i] for m in range(len(metric_indices))]

        # Compute final score based on method
        if method == "Percentiles":
            # Average of smoothed normalized values
            score = sum(user_scores) / len(user_scores)
        elif method == "Mean +/- Std":
            mean = sum(user_scores) / len(user_scores)
            std = (sum((x - mean) ** 2 for x in user_scores) / len(user_scores)) ** 0.5
            score = mean + std
            # Clamp to 0-1
            score = min(max(score, 0.0), 1.0)
        elif method == "Quartiles":
            # Use median of smoothed normalized values for simplicity
            sorted_scores = sorted(user_scores)
            mid = len(sorted_scores) // 2
            if len(sorted_scores) % 2 == 0:
                median = (sorted_scores[mid-1] + sorted_scores[mid]) / 2
            else:
                median = sorted_scores[mid]
            score = median
        else:
            score = sum(user_scores) / len(user_scores)

        results.append({"name": name, "score": round(score, 2)})

    return results

def main():
    data = json.load(sys.stdin)
    users = data.get("users", [])
    config = data.get("config", {})
    result = scale_users(users, config)
    print(json.dumps(result))

if __name__ == "__main__":
    main()
