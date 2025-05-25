import React from "react";
import {BrowserRouter, Routes, Route} from "react-router-dom";
import ExampleView from "@ui/views/ExampleView/ExampleView";
import LoginView from "@ui/views/LoginView/LoginView";
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

>>>>>>> b434a03c4ba4b9156a13ba04c7f121e5a4a362d2
