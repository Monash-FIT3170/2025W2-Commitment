import React from "react";
import NavBar from "../landing-page/NavBar";
import { Outlet } from 'react-router-dom';


export default function Layout() {
  return (
    <div className="w-screen h-screen bg-bottom flex flex-col">
          <NavBar />

          <Outlet />
    </div>
  );
}
