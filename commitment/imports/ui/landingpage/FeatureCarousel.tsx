import React, { useCallback } from 'react'
import { FeatureCard } from "./FeatureCard"
import useEmblaCarousel from 'embla-carousel-react'
import { ChevronLeft, ChevronRight } from "lucide-react"

interface Feature {
  title: string
  description: string
  image: string
  alt: string
}

const features: Feature[] = [
  {
    title: 'Repository Analytics Dashboard',
    description: 'View summary analytics for each Git repository, including activity levels and trends.',
    image: '/img/test.png',
    alt: 'Alt image.',
  },
  {
    title: 'Team Comparison View',
    description: 'Compare multiple users or groups across contribution metrics for peer review or group assessment.',
    image: '/img/test.png',
    alt: 'Alt image.',
  },
  {
    title: 'Custom Filtering',
    description: 'Filter analytics by time frame, contributor or branch to focus on different analytics.',
    image: '/img/test.png',
    alt: 'Alt image.',
  },
  {
    title: 'Moodle Gradebook Integration',
    description: 'As an educator, upload a gradebook to apply scaling based on metrics for streamlined grading.',
    image: '/img/test.png',
    alt: 'Alt image.',
  },
  {
    title: 'Per-User Contribution Insights',
    description: 'Track individual contributions such as commit frequency and lines of code.',
    image: '/img/test.png',
    alt: 'Alt image.',
  },
]



export function FeatureCarousel() {
  const [emblaRef, emblaApi] = useEmblaCarousel({ loop: true })

  const scrollPrev = useCallback(() => {
    if (emblaApi) emblaApi.scrollPrev()
  }, [emblaApi])

  const scrollNext = useCallback(() => {
    if (emblaApi) emblaApi.scrollNext()
  }, [emblaApi])

  return (
    <div className="relative w-full overflow-hidden" ref={emblaRef}>
      <div className="flex touch-pan-x will-change-transform">
        {features.map((feature, index) => (
          <div
            className="flex-[0_0_80%] md:flex-[0_0_33.3333%] px-4"
            key={index}
          >
            <FeatureCard
              title={feature.title}
              description={feature.description}
              image={feature.image}
              alt={feature.alt}
            />
          </div>
        ))}
      </div>

      <button
        onClick={scrollPrev}
        className="absolute left-2 top-1/2 transform -translate-y-1/2 bg-orange-500 text-white hover:bg-orange-600 transition-colors rounded-full p-3 shadow-lg z-10"
        aria-label="Previous slide"
      >
        <ChevronLeft size={20} />
      </button>
      <button
        onClick={scrollNext}
        className="absolute right-2 top-1/2 transform -translate-y-1/2 bg-orange-500 text-white hover:bg-orange-600 transition-colors rounded-full p-3 shadow-lg z-10"
        aria-label="Next slide"
      >
        <ChevronRight size={20} />
      </button>
    </div>
  )
}
