import React from "react";
import { UserCircle } from "lucide-react";

const imageCard = (src: string, alt: string, caption?: string): React.ReactElement => (
  <figure className="overflow-hidden rounded-lg border border-git-stroke-primary bg-git-bg-elevated shadow-lg">
    <img src={src} alt={alt} className="h-auto w-full rounded-lg" />
    {caption && (
      <figcaption className="border-t border-git-stroke-secondary bg-git-bg-secondary px-4 py-2 text-sm text-git-text-secondary">
        {caption}
      </figcaption>
    )}
  </figure>
);

const AccountManagementTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <section
        id="section-2-1"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <div className="space-y-3">
          <div className="flex items-center gap-3">
            <UserCircle className="h-6 w-6 text-git-accent-primary" />
            <h2 className="text-2xl font-semibold text-git-text-primary">
              2.0 Account Management and Personalisation
            </h2>
          </div>
          <p className="text-git-text-secondary">
            There are various features exclusive to registered users:
          </p>
          <ul className="list-disc space-y-1 pl-6 text-git-text-secondary">
            <li>
              <span className="font-semibold text-git-text-primary">Alias configuration</span> allows
              mapping multiple accounts and emails to a single individual (see Section 5.0).
            </li>
            <li>
              <span className="font-semibold text-git-text-primary">Grading sheet</span> applies
              generated scalings to a Moodle grading sheet (see Section 6.0).
            </li>
            <li>
              <span className="font-semibold text-git-text-primary">Custom scaling</span> enables
              uploading scripts to generate custom scalings (see Section 4.0).
            </li>
          </ul>
        </div>

        <div className="space-y-4">
          <h3 className="text-xl font-semibold text-git-text-primary">2.1 Account Creation and Sign Up</h3>
          <p className="text-git-text-secondary">
            On the navigation bar select the <span className="font-semibold text-git-text-primary">Sign Up</span> button
            to navigate to the sign up page.
          </p>
          {imageCard(
            "/docs_images/accountman/Picture15.png",
            "Navigation bar highlighting the Sign Up button."
          )}
          <p className="text-sm text-git-text-secondary">Location of the Sign Up button</p>
          <p className="text-git-text-secondary">
            On the sign up page, there are three options for registering an account:
          </p>
          <ul className="list-disc space-y-1 pl-6 text-git-text-secondary">
            <li>Through email and password</li>
            <li>Through Google OAuth</li>
            <li>Through GitHub OAuth</li>
          </ul>
          {imageCard(
            "/docs_images/accountman/Picture16.png",
            "Sign up form showing email, password, Google, and GitHub options."
          )}
        </div>
      </section>

      <section
        id="section-2-2"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h3 className="text-xl font-semibold text-git-text-primary">2.2 Login</h3>
        <p className="text-git-text-secondary">
          Enter the email and password associated with your account, or sign in using Google or GitHub
          OAuth.
        </p>
        {imageCard(
          "/docs_images/accountman/Picture17.png",
          "Login form with email, password, and OAuth options."
        )}
      </section>

      <section
        id="section-2-3"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h3 className="text-xl font-semibold text-git-text-primary">2.3 Sign Out</h3>
        <p className="text-git-text-secondary">
          Open the profile menu on the navigation bar and choose <span className="font-semibold text-git-text-primary">Sign Out</span>.
        </p>
        {imageCard(
          "/docs_images/accountman/Picture18.png",
          "Profile menu showing the Sign Out action."
        )}
      </section>

      <section
        id="section-2-4"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h3 className="text-xl font-semibold text-git-text-primary">2.4 Personalisation</h3>
        <p className="text-git-text-secondary">
          Use the theme toggle on the navigation bar to switch between light and dark modes.
        </p>
        {imageCard(
          "/docs_images/accountman/Picture19.png",
          "Navigation bar theme toggle highlighted."
        )}
        {imageCard(
          "/docs_images/accountman/Picture20.png",
          "Example of the interface after changing the theme."
        )}
      </section>
    </div>
  );
};

export default AccountManagementTab;
