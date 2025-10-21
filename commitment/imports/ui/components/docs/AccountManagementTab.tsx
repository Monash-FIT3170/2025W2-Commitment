import React from "react";
import { UserCircle, LogIn, UserPlus, Settings } from "lucide-react";

const AccountManagementTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
        <div className="flex items-center gap-2 mb-4">
          <UserCircle className="h-6 w-6 text-git-accent-primary" />
          <h2 className="text-2xl font-semibold text-git-text-primary">2.0 Account Management and Personalisation</h2>
        </div>
        <p className="text-git-text-secondary mb-6">
          Manage your account, personalize your experience, and access authenticated features.
        </p>

        {/* Section 2.1 */}
        <div className="mb-6" id="section-2-1">
          <h3 className="text-xl font-semibold text-git-text-primary mb-3 flex items-center gap-2">
            <UserPlus className="h-5 w-5 text-git-accent-primary" />
            <span className="text-git-accent-primary">2.1</span> Account Creation
          </h3>
          <div className="ml-6 space-y-3 text-git-text-secondary">
            <p>How to create a new account and get started with Commitment.</p>
            <ul className="list-disc list-inside ml-4 space-y-1">
              <li>Sign up process</li>
              <li>Email verification</li>
              <li>Setting up your profile</li>
            </ul>
          </div>
        </div>

        {/* Section 2.2 */}
        <div className="mb-6" id="section-2-2">
          <h3 className="text-xl font-semibold text-git-text-primary mb-3 flex items-center gap-2">
            <LogIn className="h-5 w-5 text-git-accent-primary" />
            <span className="text-git-accent-primary">2.2</span> Login
          </h3>
          <div className="ml-6 space-y-3 text-git-text-secondary">
            <p>Access your account and manage sessions.</p>
            <ul className="list-disc list-inside ml-4 space-y-1">
              <li>Login with email and password</li>
              <li>OAuth authentication options</li>
              <li>Password recovery</li>
              <li>Logout procedure</li>
            </ul>
          </div>
        </div>

        {/* Section 2.3 */}
        <div id="section-2-3">
          <h3 className="text-xl font-semibold text-git-text-primary mb-3 flex items-center gap-2">
            <Settings className="h-5 w-5 text-git-accent-primary" />
            <span className="text-git-accent-primary">2.3</span> Personalisation
          </h3>
          <div className="ml-6 space-y-4 text-git-text-secondary">
            <p className="mb-3">Features available to signed-in users:</p>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Bookmarks and Dashboard</h4>
              <p>Save and manage your favorite repositories for quick access.</p>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Custom Scaling</h4>
              <p>Create personalized scaling functions for your analysis needs.</p>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Alias Management</h4>
              <p>Configure contributor aliases to consolidate identities.</p>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Grading Sheet Upload</h4>
              <p>Upload and manage grading sheets for educational purposes.</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AccountManagementTab;
