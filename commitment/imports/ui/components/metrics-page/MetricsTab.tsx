import { Tabs, TabsList, TabsTrigger, TabsContent } from "@ui/components/ui/tabs";

export function MetricsTabs() {
  return (
    <Tabs defaultValue="overview" className="w-full">
      <TabsList className="flex justify-center bg-white p-2 rounded-xl border mb-6 gap-2">
        <TabsTrigger
          value="overview"
          className="px-4 py-2 rounded-md text-sm font-medium data-[state=active]:bg-[#F1502F] data-[state=active]:text-white data-[state=active]:shadow-sm border border-[#F1502F] text-[#F1502F]"
        >
          Overview
        </TabsTrigger>
        <TabsTrigger
          value="metrics"
          className="px-4 py-2 rounded-md text-sm font-medium data-[state=active]:bg-[#F1502F] data-[state=active]:text-white data-[state=active]:shadow-sm border border-[#F1502F] text-[#F1502F]"
        >
          Metrics
        </TabsTrigger>
        <TabsTrigger
          value="scaling"
          className="px-4 py-2 rounded-md text-sm font-medium data-[state=active]:bg-[#F1502F] data-[state=active]:text-white data-[state=active]:shadow-sm border border-[#F1502F] text-[#F1502F]"
        >
          Scaling
        </TabsTrigger>
        <TabsTrigger
          value="contributions"
          className="px-4 py-2 rounded-md text-sm font-medium data-[state=active]:bg-[#F1502F] data-[state=active]:text-white data-[state=active]:shadow-sm border border-[#F1502F] text-[#F1502F]"
        >
          Contributions
        </TabsTrigger>
      </TabsList>

      <TabsContent value="overview">
        {/* OVERVIEW */}
      </TabsContent>
      <TabsContent value="metrics">
        {/* METRICS */}
      </TabsContent>
      <TabsContent value="scaling">
        {/* SCALING */}
      </TabsContent>
      <TabsContent value="contributions">
        {/* CONTRIBUTIONS */}
      </TabsContent>
    </Tabs>
  );
}
