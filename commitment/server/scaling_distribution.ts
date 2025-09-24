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
 * @param passedContributor the contributor to get the percentile for
 * @param data the filtered data
 * @param selectedMetric the metric to get the percentile for
 * @returns ContributorScaledMetric a data structure containing the metric, value and percentile
 */
export function getContributorScaledMetric(passedContributor: SerialisableMapObject<string, ContributorData>, data: FilteredData, selectedMetric: MetricType): ContributorScaledMetric {   
    // get all contributor metrics
    const contributorMetrics = getContributorMetrics(data);

    // get the metric values for all contributors 
    const metricValues = contributorMetrics.map((contributor) => contributor.metrics[selectedMetric]);
    
    // get the value for the passed contributor 
    const contributorName = passedContributor.value.name;
   
    // find the contributor in the metrics array
    const contributorMetric = contributorMetrics.find((contributor) => contributor.contributorName === contributorName);
    if (!contributorMetric) {
        throw new Error(`Contributor ${contributorName} not found in metrics.`);
    }
    // get the contributor's value for the selected metric
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

/**
 * This function provides the scaled data for each contributor in the filtered data.
 * @param data the filtered data
 * @param selectedMetric the metric to get the scaled data for
 * @returns an array of ContributorScaledData
 */
export function getContributorScaledData(data: FilteredData, selectedMetric:MetricType): ContributorScaledData[]{
    const { repositoryData, repositoryData: { contributors } } = data;
    
    return contributors.map((contributor) => {
        const scaledMetric = getContributorScaledMetric(contributor, data, selectedMetric);
        return {
            contributorName: contributor.value.name,
            scaledMetrics: [scaledMetric],
        };
    });
}