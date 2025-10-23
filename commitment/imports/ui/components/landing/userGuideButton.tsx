import React from "react";
import { Button } from "@base/button";

const USER_GUIDE_URL =
  "https://docs.google.com/document/d/1xCvr5bou8CQLwafXJQv6Hn8T8YnQ_b0p2EabGmSpzMk/edit?usp=sharing";

const UserGuideButton: React.FC = () => (
  <div className="flex flex-col items-center gap-3 text-center">
    <span className="text-sm font-medium text-git-text-secondary">
      Need more help?
    </span>
    <Button
      size="lg"
      variant="secondary"
      type="button"
      onClick={() =>
        window.open(USER_GUIDE_URL, "_blank", "noopener,noreferrer")
      }
      className="bg-git-int-primary text-git-text-primary hover:bg-git-int-secondary-hover rounded-4xl text-center"
    >
      <span className="text-md font-bold text-white">
        View our User Guides here!
      </span>
    </Button>
  </div>
);

export default UserGuideButton;
