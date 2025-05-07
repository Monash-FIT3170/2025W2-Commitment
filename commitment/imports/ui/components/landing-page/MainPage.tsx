import React from "react";
import { Button } from "@ui/components/ui/button"


const MainPage = () => (
  <div className="flex flex-col gap-6">
    <div className="flex justify-center items-center mb-4">
      <Logo />
    </div>

    <div className="flex justify-center items-center mb-6">
      <GetStartedButton />
    </div>

    <div className="flex justify-center items-center">
        <Description/>
    </div>
  </div>
);

export default MainPage;

const Logo = () => (
  <div className="flex flex-row items-center">
    <img src="/logo.png" alt="Logo" className="-mr-7 w-[120px] h-auto" />
    <p className="text-8xl text-gray-700 ">ommitment</p>

  </div>
);

const Description =  () => (
    <p className="text-center flex text-lg">Need to quickly understand how committed your students are to their group projects? <br/> We got you.</p>

);

const GetStartedButton = () => (
    <Button className="w-[341px] h-auto text-[36px] rounded-full  text-center bg-[#F1502F] hover:bg-[#F1502F]  drop-shadow-lg" >Get Started</Button>
    // <button
    // className="w-[341px] h-[87px] bg-cover bg-center rounded-full flex items-center justify-center">
    // <p className="text-[36px] text-gray-700 ">Get Started</p>
    
    // </button>

);
