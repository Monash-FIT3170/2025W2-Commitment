import React from "react";
import { Logo } from "@ui/components/landing-page/MainPage";
import LastSavedRepos from "@ui/components/insert-git-repo/LastSavedRepos";
import GitRepoInputSection from "@ui/components/insert-git-repo/GitRepoInputSection";
import { Meteor } from "meteor/meteor";
import { useTracker } from "meteor/react-meteor-data";

const InsertGitRepoView: React.FC = () => {
  const user = useTracker(() => Meteor.user());
  const isLoggedIn = !!user;

  return (
    <>

      {isLoggedIn ? (
        <div className="flex flex-col items-center pt-20">
          <h1 className="text-6xl text-gray-700 mb-8">
            Welcome Back,
            {user?.profile?.name || "User"}
          </h1>
          <GitRepoInputSection />
          <LastSavedRepos />
        </div>
      ) : (
        <div className="flex flex-col items-center pt-20">
          <div className="mb-4">
            <Logo />
          </div>
          <p className="text-center text-lg text-gray-700 mb-8">
            Quick Git metrics for overworked TAs
          </p>
          <GitRepoInputSection />
          <p className="text-center text-lg text-gray-700 mt-8">
            Need to quickly understand how committed your students are to their
            group? We got you
          </p>
        </div>
      )}
    </>
  );
};

export default InsertGitRepoView;
