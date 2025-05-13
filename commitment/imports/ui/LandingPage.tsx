import React from "react";
import Purpose from "@ui/components/landing-page/PurposePage";
import MainPage from "@ui/components/landing-page/MainPage";
import AboutUs from "@ui/components/landing-page/AboutUs";
import Footer from "@ui/components/landing-page/Footer";
import NavBar from "@ui/components/landing-page/NavBar";
import Features from "@ui/components/landing-page/Features";

export const LandingPage = () => (
  <div className="m-0">
    <div className="flex flex-col gap-32">
      {/* Put components here */}
      <NavBar />
      <MainPage />
      <AboutUs/>
      <Features/>
      <Purpose />
      <Footer />
    </div>
  </div>
);
