import React from "react";
import {BrowserRouter, Routes, Route} from "react-router-dom";
import ExampleView from "@ui/views/ExampleView/ExampleView";
import LoginView from "@ui/views/LoginView/LoginView";


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

