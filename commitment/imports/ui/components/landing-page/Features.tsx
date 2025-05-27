import React from "react";
import { FeatureCarousel } from "./FeatureCarousel";

export const Features = () => {
  return (
    <div className="ml-32 mr-32">
      <div className="w-full text-center mt-10">
        <p className="text-sm text-[#F1502F] font-medium font-robotoFlex tracking-wide">
          OUR FEATURES?
        </p>
        <h1 className="text-5xl text-gray-900 mt-2 font-robotoFlex">
          Get The Full Picture...
        </h1>
      </div>
      <FeatureCarousel />
    </div>
  );
}

export default Features;