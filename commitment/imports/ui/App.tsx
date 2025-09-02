import React from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import LandingPage from "@ui/LandingPage";
import LoginView from "@ui/views/LoginView/LoginView";
import InsertGitRepoView from "@ui/views/InsertGitRepoView/InsertGitRepo";
import LoadingPage from "./LoadingPage";
import MetricsPage from "./MetricsPage";
import DashboardView from "./views/DashboardView/DashboardView";
import Authorized from "@ui/components/shared/Authorized";
import SettingsPage from "./components/settings/SettingsPage";

export default function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<LandingPage />} />
        <Route path="/login" element={<LoginView />} />
        <Route path="/signup" element={<LoginView defaultTab={"signup"}/>} />
        <Route path="/home" element={<InsertGitRepoView />} />

        {/* Add more routes as needed */}

        <Route path="/loading" element={<LoadingPage />} />
        <Route path="/metrics" element={<MetricsPage />} />
        <Route path="/dashboard" element={
          <Authorized>
            <DashboardView/>
          </Authorized>
        }/>
        <Route path="/settings" element={<SettingsPage />} />
      </Routes>
    </BrowserRouter>
  );
}
