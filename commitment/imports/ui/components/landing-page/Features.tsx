import React from "react";
import { FeatureCarousel } from "./FeatureCarousel";

export const Features = () => {
  return (
    <div className="ml-32 mr-32">
      <div className="w-full text-center mt-10">
        <p className="text-git-500 text-lg font-semibold font-mono tracking-wide">
          OUR FEATURES?
        </p>
        <h1 className="text-5xl mt-2 font-mono">Get The Full Picture...</h1>
      </div>
      <FeatureCarousel />
    </div>
  );
};

export default Features;
