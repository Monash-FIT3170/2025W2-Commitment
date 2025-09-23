import React from "react";
import { Outlet } from 'react-router-dom';
import NavBar from "../landing-page/NavBar";


export default function Layout() {
  return (
    <div className="w-full h-screen bg-git-bg-elevated">
          <NavBar />

          <Outlet />
    </div>
  );
}
