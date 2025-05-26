import React from "react";
import {BrowserRouter, Routes, Route} from "react-router-dom";
import { LandingPage } from "@ui/LandingPage";
import LoginView from "@ui/views/LoginView/LoginView";
<<<<<<< HEAD
<<<<<<< HEAD

export const App = () => {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<ExampleView/>}></Route>
        <Route path="/login" element={<LoginView/>}></Route>
      </Routes>
    </BrowserRouter>
  )
}
=======

=======
import InsertGitRepoView from '@ui/views/InsertGitRepoView/InsertGitRepo';
>>>>>>> 98df114705301a679a1d5b13273d636d58df94be

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

>>>>>>> b434a03c4ba4b9156a13ba04c7f121e5a4a362d2
