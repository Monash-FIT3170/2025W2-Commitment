import React, { useEffect, useCallback, useState } from "react";
import useEmblaCarousel from "embla-carousel-react";
import { ChevronLeft, ChevronRight } from "lucide-react";

interface FeatureCardProps {
  title: string;
  description: string;
  image: string;
  alt: string;
  className?: string;
}

export const FeatureCard = ({
  title,
  description,
  image,
  alt,
  className = "",
}: FeatureCardProps) => {
  return (
    <div
      className={`flex flex-col justify-between 
        border-2 border-git-card-primary bg-white rounded-xl p-5 text-center 
        w-full h-full 
        max-w-sm sm:max-w-md lg:max-w-lg xl:max-w-xl
        transition-all duration-300 ease-in-out
        ${className}`}
    >
      <div>
        <h2 className="text-lg sm:text-xl lg:text-2xl font-mono font-semibold text-black mb-4">
          {title}
        </h2>
        <img
          src={image}
          alt={alt}
          className="object-cover rounded-md mb-4 scale-75 sm:scale-90 lg:scale-100 mx-auto"
        />
      </div>
      <p className="text-gray-600 text-sm sm:text-base font-mono">
        {description}
      </p>
    </div>
  );
};

interface Feature {
  title: string;
  description: string;
  image: string;
  alt: string;
}

const features: Feature[] = [
  {
    title: "Repository Analytics Dashboard",
    description:
      "View summary analytics for each Git repository, including activity levels and trends.",
    image: "/feature-placeholder.svg",
    alt: "Alt image.",
  },
  {
    title: "Team Comparison View",
    description:
      "Compare multiple users across contribution metrics for peer review or group assessment.",
    image: "/feature-placeholder.svg",
    alt: "Alt image.",
  },
  {
    title: "Custom Filtering",
    description:
      "Filter analytics by time frame, contributor or branch to focus on different analytics.",
    image: "/feature-placeholder.svg",
    alt: "Alt image.",
  },
  {
    title: "Moodle Gradebook Integration",
    description:
      "As an educator, upload a gradebook to apply scaling based on metrics for streamlined grading.",
    image: "/feature-placeholder.svg",
    alt: "Alt image.",
  },
  {
    title: "Per-User Contribution Insights",
    description:
      "Track individual contributions such as commit frequency and lines of code.",
    image: "/feature-placeholder.svg",
    alt: "Alt image.",
  },
];

export function FeatureCarousel() {
  const [emblaRef, emblaApi] = useEmblaCarousel({
    loop: true,
    align: "center",
  });

  const [selectedIndex, setSelectedIndex] = useState(0);

  const scrollPrev = useCallback(() => emblaApi?.scrollPrev(), [emblaApi]);
  const scrollNext = useCallback(() => emblaApi?.scrollNext(), [emblaApi]);

  useEffect(() => {
    if (!emblaApi) return;

    const onSelect = () => setSelectedIndex(emblaApi.selectedScrollSnap());
    emblaApi.on("select", onSelect);
    onSelect();
  }, [emblaApi]);

  useEffect(() => {
    if (!emblaApi) return;
    const interval = setInterval(() => emblaApi.scrollNext(), 10000);
    return () => clearInterval(interval);
  }, [emblaApi]);

  return (
    <div className="relative w-full overflow-hidden px-4 sm:px-8 lg:px-16 mt-10">
      <div className="overflow-hidden" ref={emblaRef}>
        <div className="flex">
          {features.map((feature, index) => {
            const isSelected = index === selectedIndex;

            return (
              <div
                key={index}
                className={`
                  shrink-0 px-2 ease-in-out 
                  ${isSelected ? "opacity-100 blur-none" : "opacity-50 blur-xs"}
                `}
                style={{
                  width: "33.3333%", // Always 3 cards
                  scrollSnapAlign: "center",
                }}
              >
                <FeatureCard
                  title={feature.title}
                  description={feature.description}
                  image={feature.image}
                  alt={feature.alt}
                  className={isSelected ? "shadow-xl" : ""}
                />
              </div>
            );
          })}
        </div>
      </div>

      {/* Nav buttons */}
      <button
        onClick={scrollPrev}
        className="absolute left-[25%] top-1/2 transform -translate-y-1/2 bg-white text-[#F1502F] border-2 border-[#F1502F] rounded-full p-3 z-10"
        aria-label="Previous slide"
      >
        <ChevronLeft size={30} />
      </button>
      <button
        onClick={scrollNext}
        className="absolute right-[25%] top-1/2 transform -translate-y-1/2 bg-white text-[#F1502F] border-2 border-[#F1502F] rounded-full p-3 z-10"
        aria-label="Next slide"
      >
        <ChevronRight size={30} />
      </button>

      {/* Dots */}
      <div className="flex justify-center mt-6 gap-2">
        {features.map((_, index) => (
          <div
            key={index}
            className={`h-3 w-3 rounded-full ${
              index === selectedIndex ? "bg-git" : "bg-[#F1502F]/30"
            }`}
          />
        ))}
      </div>
    </div>
  );
}

