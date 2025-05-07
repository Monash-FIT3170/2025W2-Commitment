import React from "react";
import { Separator } from "@ui/components/ui/separator"

const purpose = () => {
  return (
    <div className="flex flex-col justify-center items-center">
      <Header />

      <CircleNumber />
    </div>
  );
};

export default purpose;

const Header = () => {
  return (
    <div className="flex flex-col justify-center items-center">
      <h4 className="text text-orange-400 font-semibold">HOW TO USE?</h4>
      <h1 className="text-3xl">Get Started in Three Steps</h1>
    </div>
  );
};

const CircleNumber = () => {
  return (
    <div className="flex flex-row pt-7 items-center gap-2 justify-center">
      <div className="w-12 h-12 bg-gray-200 rounded-full flex justify-center items-center text-center p-5 text-xl">
        1
      </div>
      <Separator decorative/>
      <div className="w-12 h-12 bg-gray-200 rounded-full flex justify-center items-center text-center p-5 text-xl">
        2
      </div>
      <Separator decorative/>
      <div className="w-12 h-12 bg-gray-200 rounded-full flex justify-center items-center text-center p-5 text-xl">
        3
      </div>
    </div>
  );
};
