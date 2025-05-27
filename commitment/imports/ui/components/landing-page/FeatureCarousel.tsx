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
        border-2 border-git bg-white rounded-xl p-5 text-center 
        w-[350px]
        ${className}`}
    >
      <div>
        <h2 className="text-xl font-semibold text-gray-800 mb-4">{title}</h2>
        <img
          src={image}
          alt={alt}
          className="scale-75 object-cover rounded-md mb-4"
        />
      </div>
      <p className="text-gray-600 text-sm font-robotoFlex">{description}</p>
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
  const [emblaRef, emblaApi] = useEmblaCarousel({ loop: true });
  const [selectedIndex, setSelectedIndex] = useState(0);

  const scrollPrev = useCallback(() => {
    if (emblaApi) emblaApi.scrollPrev();
  }, [emblaApi]);

  const scrollNext = useCallback(() => {
    if (emblaApi) emblaApi.scrollNext();
  }, [emblaApi]);

  useEffect(() => {
    if (!emblaApi) return;

    const onSelect = () => {
      setSelectedIndex(emblaApi.selectedScrollSnap());
    };

    emblaApi.on("select", onSelect);
    onSelect();
  }, [emblaApi]);

  useEffect(() => {
    if (!emblaApi) return;

    const interval = setInterval(() => {
      emblaApi.scrollNext();
    }, 10000);

    return () => clearInterval(interval);
  }, [emblaApi]);

  return (
    <div
      className="relative ml-32 mr-32 mt-10 align-middle overflow-hidden"
      ref={emblaRef}
    >
      <div className="flex">
        {features.map((feature, index) => {
          const isSelected = index === selectedIndex;
          return (
            <div
              key={index}
              className={`ease-in-out px-2 ${
                isSelected
                  ? "w-1/3 opacity-100 blur-none"
                  : "w-1/3 opacity-45 blur-sm"
              }`}
            >
              <FeatureCard
                title={feature.title}
                description={feature.description}
                image={feature.image}
                alt={feature.alt}
                className={isSelected ? "shadow-[0_4px_20px_rgba(0,0,0,0.4)]" : "shadow-none"}
              />
            </div>
          );
        })}
      </div>

      <button
        onClick={scrollPrev}
        className="absolute left-[12.5%] top-1/2 transform -translate-y-1/2 bg-white text-git border-2 border-git rounded-full p-3 z-10"
        aria-label="Previous slide"
      >
        <ChevronLeft size={30} />
      </button>
      <button
        onClick={scrollNext}
        className="absolute right-[12.5%] top-1/2 transform -translate-y-1/2 bg-white text-git border-2 border-git rounded-full p-3 z-10"
        aria-label="Next slide"
      >
        <ChevronRight size={30} />
      </button>
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
