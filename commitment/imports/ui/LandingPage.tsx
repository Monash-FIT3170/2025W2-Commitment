import React from "react";
import Purpose from "@ui/components/landing-page/PurposePage";
import MainPage from "@ui/components/landing-page/MainPage";
import AboutUs from "@ui/components/landing-page/AboutUs";
import Footer from "@ui/components/landing-page/Footer";
import NavBar from "@ui/components/landing-page/NavBar";
import Features from "@ui/components/landing-page/Features";

export default function LandingPage() {

  return (
    <div className="m-0 scroll-smooth">
      <div className="flex flex-col gap-32">
        {/* Put components here */}
        <NavBar  />
        <MainPage />
        <div id="about" className="scroll-mt-24">
          <AboutUs />
        </div>
        <div id="features" className="scroll-mt-24">
          <Features />
        </div>
        <div id="howto" className="scroll-mt-24">
          <Purpose />
        </div>
        <Footer />
      </div>
    </div>
  );
}
