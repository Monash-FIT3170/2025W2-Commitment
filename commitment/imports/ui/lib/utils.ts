import { clsx, type ClassValue } from "clsx";
import { twMerge } from "tailwind-merge";

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export function topContributors(
  dataset: { name: string; commits: number }[],
  topUsers = 5
): { name: string; commits: number }[] {
  return [...dataset].sort((a, b) => b.commits - a.commits).slice(0, topUsers);
}
