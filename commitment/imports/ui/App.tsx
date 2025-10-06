import React from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import Authorized from "@ui/components/shared/Authorized";
import LandingPage from "./views/LandingView";
import LoginView from "/imports/ui/views/LoginView";
import InsertGitRepoView from "./views/HomeView";
import LoadingPage from "./views/LoadingView";
import MetricsPage from "./views/MetricsView";
import DashboardView from "./views/DashboardView";
import SettingsPage from "./components/settings/SettingsPage";
import Layout from "./components/shared/Layout";
import DeveloperDocsView from "./views/DeveloperDocsView";

export default function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Layout />}>
          <Route index element={<LandingPage />} />
          <Route path="/login" element={<LoginView />} />
          <Route path="/signup" element={<LoginView defaultTab="signup" />} />
          <Route path="/home" element={<InsertGitRepoView />} />

          {/* Add more routes as needed */}

          <Route path="/loading" element={<LoadingPage />} />
          <Route path="/metrics" element={<MetricsPage />} />
          <Route path="/developer-docs" element={<DeveloperDocsView />} />
          <Route
            path="/dashboard"
            element={
              <Authorized>
                <DashboardView />
              </Authorized>
            }
          />
                  <Route path="/settings" element={<Authorized>
          <SettingsPage /></Authorized>
        } />
        </Route>

      </Routes>
    </BrowserRouter>
  );
}
