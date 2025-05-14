import React from "react";
import {BrowserRouter, Routes, Route} from "react-router-dom";
import ExampleView from "@ui/views/ExampleView/ExampleView";
import LoginView from "@ui/views/LoginView/LoginView";
import DashboardView from "./views/DashboardView/DashboardView";

export const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<ExampleView/>}></Route>
        <Route path="/login" element={<LoginView/>}></Route>
        <Route path="/dashboard" element={<DashboardView/>}></Route>
      </Routes>
    </BrowserRouter>
  )
}