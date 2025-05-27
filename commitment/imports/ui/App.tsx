import React from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import { LandingPage } from "@ui/LandingPage";
import LoginView from "@ui/views/LoginView/LoginView";
import InsertGitRepoView from "@ui/views/InsertGitRepoView/InsertGitRepo";
import { MetricsMain } from "./MetricsMain";

export const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<LandingPage />}></Route>
        <Route path="/login" element={<LoginView />}></Route>
        <Route path="/home" element={<InsertGitRepoView />}></Route>
        <Route path="/metrics" element={<MetricsMain />}></Route>
      </Routes>
    </BrowserRouter>
  );
};
