import React from "react";
import { Outlet } from 'react-router-dom';
import NavBar from "./NavBar";


export default function Layout() {
  return (
    <div className="w-full h-screen">
          <NavBar />

          <Outlet />
    </div>
  );
}
