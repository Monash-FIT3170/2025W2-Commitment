// This file will consist of functions that returns the desired input for the contributor metric-scaling distribution graph
 
// function to produce contributor metrics 
// need to use get filtered data as the input to do so . 
import {ContributorMetrics, ContributorScaledData, ContributorScaledMetric, FilteredData, MetricType, SerialisableMapObject, ContributorData } from "../imports/api/types"
import { getCommitPerDayPerContributor, getTotalCommitsPerContributor, getLOCperContributor, getLocPerCommitPerContributor } from "./helper_functions";

/**
 * This function provides the metrics for each contributor in the filtered data.
 * @param data 
 * @returns 
 */
export function getContributorMetrics(data: FilteredData): ContributorMetrics[]{
    // for each contributor in the filtered data, find the metric associated to them:
    const { repositoryData, repositoryData: { contributors } } = data;

    // loop through each contributor and get their metric 
    return contributors.map((contributor) => ({
        contributorName: contributor.value.name,
        metrics: {
            [MetricType.TOTAL_COMMITS]: getTotalCommitsPerContributor(repositoryData, contributor.value.name),
            [MetricType.LOC]: getLOCperContributor(repositoryData, contributor.value.name),
            [MetricType.LOC_PER_COMMIT]: getLocPerCommitPerContributor(repositoryData, contributor.value.name),
            [MetricType.COMMITS_PER_DAY]: getCommitPerDayPerContributor(repositoryData, contributor.value.name),
        },
    })); 
}

/** 
 * This function provides the percentile of a contributors metric.
 */
export function getContributorScaledMetric(passedContributor: SerialisableMapObject<string, ContributorData>, data: FilteredData, selectedMetric: MetricType): ContributorScaledMetric {    // get all contributor metrics 
    // get all contributor metrics
    const contributorMetrics = getContributorMetrics(data);

    // get the metric values for all contributors 
    const metricValues = contributorMetrics.map((contributor) => contributor.metrics[selectedMetric]);
    // get the value for the passed contributor 
    const contributorName = passedContributor.value.name;
    const contributorMetric = contributorMetrics.find((contributor) => contributor.contributorName === contributorName);
    if (!contributorMetric) {
        throw new Error(`Contributor ${contributorName} not found in metrics.`);
    }
    const contributorValue = contributorMetric.metrics[selectedMetric];

    // calculate the number of values below the contributor's value
    const numValuesBelow = metricValues.filter((value) => value < contributorValue).length;
    const totalValues = metricValues.length;
    
    // calculate the percentile
    const percentile = (numValuesBelow / totalValues) * 100;
    return {
        metric: selectedMetric,
        value: contributorValue,
        percentile,
    };  
    
}

export function getContributorScaledData(data: FilteredData, selectedMetric:MetricType): ContributorScaledData[]{
    // things happen here 
    // this would iterate through each contributor and getContributorScaledMetric for each contributor
    const { repositoryData, repositoryData: { contributors } } = data;



}