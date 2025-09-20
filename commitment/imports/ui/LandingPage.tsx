import React, { useEffect } from "react";
import Purpose from "/imports/ui/components/landing/PurposePage";
import MainPage from "/imports/ui/components/landing/MainPage";
import AboutUs from "/imports/ui/components/landing/AboutUs";
import Footer from "/imports/ui/components/landing/Footer";
import Features from "/imports/ui/components/landing/Features";

export default function LandingPage() {
  // Clear repository history when user navigates to landing page
  useEffect(() => {
    localStorage.removeItem('lastRepoUrl');
  }, []);

  return (
    <div className="m-0 scroll-smooth">
      <div className="flex flex-col gap-32">
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
