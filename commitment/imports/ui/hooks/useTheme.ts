import { useCallback, useEffect, useState } from "react";

// Avoid flashing of auto theme switching
const getInitialIsDark = () => {
  // Check saved choice first
  const saved = localStorage.getItem("theme");
  if (saved === "dark") return true;
  if (saved === "light") return false;

  // Fall back to system preference
  return window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;
};

export function useTheme() {
  const [isDark, setIsDark] = useState<boolean>(getInitialIsDark);

  // Apply to <html> whenever state changes
  useEffect(() => {
    const root = document.documentElement;
    if (isDark) {
      root.classList.add("dark");
      localStorage.setItem("theme", "dark");
    } else {
      root.classList.remove("dark");
      localStorage.setItem("theme", "light");
    }
  }, [isDark]);

  // Also respond to system changes **only if** the user hasn't explicitly chosen
  useEffect(() => {
    const saved = localStorage.getItem("theme");
    if (saved) return; // user picked explicitly; ignore system changes

    const mq = window.matchMedia("(prefers-color-scheme: dark)");
    const handler = (e: MediaQueryListEvent) => {
      // If the user later makes an explicit choice, ignore system changes
      const nowSaved = localStorage.getItem("theme");
      if (nowSaved) return;
      setIsDark(e.matches);
    };

    mq.addEventListener("change", handler);
    return () => mq.removeEventListener("change", handler);
  }, []);

  const toggle = useCallback(() => setIsDark((d) => !d), []);

  // Keep theme in sync across tabs/windows
  useEffect(() => {
    const onStorage = (e: StorageEvent) => {
     if (e.key === "theme") {
       if (e.newValue === "dark") setIsDark(true);
       if (e.newValue === "light") setIsDark(false);
       if (e.newValue == null) {
         // If cleared, fall back to system
         const mq = window.matchMedia("(prefers-color-scheme: dark)");
         setIsDark(mq.matches);
       }
     }
   };
   window.addEventListener("storage", onStorage);
   return () => window.removeEventListener("storage", onStorage);
   }, []);
  
  return { isDark, toggle };
}
