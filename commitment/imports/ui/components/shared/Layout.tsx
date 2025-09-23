import React from "react";
import { Outlet } from 'react-router-dom';
import NavBar from "../landing-page/NavBar";


export default function Layout() {
  return (
    <div className="w-screen h-screen overflow-scroll bg-git-bg-elevated">
          <NavBar />

          <Outlet />
    </div>
  );
}
