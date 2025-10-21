import React from "react";
import { Code2, ArrowLeftRight } from "lucide-react";

const CustomScalingTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
        <div className="flex items-center gap-2 mb-4">
          <Code2 className="h-6 w-6 text-git-accent-primary" />
          <h2 className="text-2xl font-semibold text-git-text-primary">4.0 Custom Scaling</h2>
        </div>
        <p className="text-git-text-secondary mb-6">
          Create custom Python functions to scale contributor metrics according to your specific requirements.
        </p>

        <div className="space-y-4 text-git-text-secondary">
          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2 flex items-center gap-2">
              <ArrowLeftRight className="h-5 w-5 text-git-accent-primary" />
              How It Works
            </h3>
            <p className="ml-6 mb-3">
              Custom scaling allows you to define your own algorithms for normalizing and scaling contributor
              metrics. This is particularly useful for educational settings where you need specific grading curves
              or normalization strategies.
            </p>
          </div>

          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2">Python Function Expectations</h3>
            <div className="ml-6">
              <p className="mb-3">Your custom scaling function must follow specific requirements:</p>

              <h4 className="font-semibold text-git-text-primary mb-2">Function Signature</h4>
              <div className="bg-git-bg-primary border border-git-stroke-secondary rounded-lg p-4 mb-4 font-mono text-sm">
                <code>
                  def scale_metrics(data: List[Dict]) -&gt; List[Dict]:
                </code>
              </div>

              <h4 className="font-semibold text-git-text-primary mb-2">Inputs</h4>
              <ul className="list-disc list-inside ml-4 space-y-1 mb-4">
                <li><code className="bg-git-bg-primary px-2 py-1 rounded">data</code>: List of dictionaries containing contributor metrics</li>
                <li>Each dictionary contains: <code className="bg-git-bg-primary px-2 py-1 rounded">contributor</code>, <code className="bg-git-bg-primary px-2 py-1 rounded">commits</code>, <code className="bg-git-bg-primary px-2 py-1 rounded">additions</code>, <code className="bg-git-bg-primary px-2 py-1 rounded">deletions</code></li>
                <li>All numeric values are guaranteed to be non-null</li>
              </ul>

              <h4 className="font-semibold text-git-text-primary mb-2">Outputs</h4>
              <ul className="list-disc list-inside ml-4 space-y-1 mb-4">
                <li>Must return a list of dictionaries with the same structure as input</li>
                <li>Each dictionary should include a <code className="bg-git-bg-primary px-2 py-1 rounded">scaled_value</code> field</li>
                <li>Scaled values typically range from 0-100, but this is not enforced</li>
                <li>Order of contributors must be preserved</li>
              </ul>

              <h4 className="font-semibold text-git-text-primary mb-2">Example Function</h4>
              <div className="bg-git-bg-primary border border-git-stroke-secondary rounded-lg p-4 font-mono text-sm">
                <pre><code>{`def scale_metrics(data):
    # Calculate total commits
    total = sum(d['commits'] for d in data)

    # Scale each contributor
    for contributor in data:
        percentage = (contributor['commits'] / total) * 100
        contributor['scaled_value'] = round(percentage, 2)

    return data`}</code></pre>
              </div>
            </div>
          </div>

          <div className="mt-6 p-4 bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-lg">
            <p className="text-yellow-800 dark:text-yellow-200 text-sm">
              <strong>Note:</strong> Custom scaling functions are executed in a sandboxed environment. Ensure your
              code is efficient and doesn't rely on external libraries not included in the standard Python environment.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
};

export default CustomScalingTab;
