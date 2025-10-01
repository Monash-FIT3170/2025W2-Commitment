import React from "react";

const AboutUs = () => (
    <div className="flex flex-col justify-center items-center mt-6 ">
      <Header />
      <Description />
      <KeyFeatureImage />
    </div>
  );

export default AboutUs;

const Header = () => (
    <div className="flex flex-col justify-center items-center">
      <h4 className="font-mono text-lg font-semibold text-git-500">
        WHAT IS COMMITMENT?
      </h4>
      <h1 className="text-5xl font-mono">Your Contribution Analyser </h1>
    </div>
  );

const Description = () => (
    <div className="flex flex-col justify-center items-center mt-6 max-w-xl mx-auto">
      <p className="text-sm text-center font-mono">
        {" "}
        In team-based coding environments, it can be difficult to understand who
        did what, when, and how much. Commitment solves this by analyzing Git
        repositories and turning raw commit data into insightful visualizations.
      </p>
    </div>
  );

const KeyFeatureImage = () => (
    <div className="mt-6">
      <img
        src="/feature.png"
        alt="Features of Commitment."
        className=" rounded-xl max-w-4xl mx-auto"
      />
    </div>
  );
