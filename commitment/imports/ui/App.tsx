import React from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import { LandingPage } from "@ui/LandingPage";
import LoginView from "@ui/views/LoginView/LoginView";
import InsertGitRepoView from '@ui/views/InsertGitRepoView/InsertGitRepo';
import LoadingPage from "./LoadingPage";
import MetricsMain from "./MetricsMain";
import DashboardView from "./views/DashboardView/DashboardView";


export const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<LandingPage/>}></Route>
        <Route path="/login" element={<LoginView/>}></Route>
        <Route path="/signup" element={<LoginView defaultTab={"signup"}/>}></Route>
        <Route path="/home" element={<InsertGitRepoView/>}></Route>

        {/* Add more routes as needed */}

        <Route path="/loading" element={<LoadingPage/>}></Route>
        <Route path="/metrics" element={<MetricsMain />}></Route>
        <Route path="/dashboard" element={<DashboardView/>}></Route>
        
      </Routes>
    </BrowserRouter>
  );
};
