import React from "react";
import {BrowserRouter, Routes, Route} from "react-router-dom";
import { LandingPage } from "@ui/LandingPage";
import LoginView from "@ui/views/LoginView/LoginView";
import InsertGitRepoView from '@ui/views/InsertGitRepoView/InsertGitRepo';

export const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<LandingPage/>}></Route>
        <Route path="/login" element={<LoginView/>}></Route>
        <Route path="/home" element={<InsertGitRepoView/>}></Route>
      </Routes>
    </BrowserRouter>
  )
}

