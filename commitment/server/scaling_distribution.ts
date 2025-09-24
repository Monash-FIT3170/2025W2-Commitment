import {ContributorMetrics, ContributorScaledData, ContributorScaledMetric, FilteredData, MetricType, SerialisableMapObject, ContributorData, RepoMetricDistribution, ScalingDistributionResult } from "../imports/api/types"
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

/**
 * This function provides the distribution of a selected metric across all contributors in the filtered data.
 * @param data The repository filtered data 
 * @param selectedMetric the metric to get the distribution for
 * @returns A data structure containing the distribution of the selected metric across all contributors
 */
export function getRepoMetricDistribution(data: FilteredData, selectedMetric: MetricType): RepoMetricDistribution {
    // get all contributor metrics
    const contributorMetrics = getContributorMetrics(data);

    // get the metric values for all contributors 
    const metricValues = contributorMetrics.map((contributor) => contributor.metrics[selectedMetric]);

    // sort the metric values
    metricValues.sort((a, b) => a - b);
    
    // calculate min, Q1, median, Q3, max, mean
    const min = metricValues[0];
    const max = metricValues[metricValues.length - 1];
    const mean = metricValues.reduce((acc, val) => acc + val, 0) / metricValues.length;
    const median = metricValues.length % 2 === 0 ?
        (metricValues[metricValues.length / 2 - 1] + metricValues[metricValues.length / 2]) / 2 :
        metricValues[Math.floor(metricValues.length / 2)];
    const Q1 = metricValues[Math.floor((metricValues.length / 4))];
    const Q3 = metricValues[Math.floor((metricValues.length * 3) / 4)];

    return {
        metric: selectedMetric,
        min,
        Q1,
        median,
        Q3,
        max,
        mean,
    };
}

/**
 * This function provides the scaling distribution result for the selected metric across all contributors in the filtered data.
 * @param data the filtered data
 * @param selectedMetric the selected metric
 * @returns the scaling distribution result
 */
export function getScalingDistributionResult(data: FilteredData, selectedMetric: MetricType): ScalingDistributionResult{
    // get scaled data for each selected metric
    return {
        contributors: getContributorScaledData(data, selectedMetric),
        repoDistributions: getRepoMetricDistribution(data, selectedMetric),
    }
}
