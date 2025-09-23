// This file will consist of functions that returns the desired input for the contributor metric-scaling distribution graph
 
// function to produce contributor metrics 
// need to use get filtered data as the input to do so . 
import {ContributorMetrics, ContributorScaledData, ContributorScaledMetric, FilteredData, MetricType } from "../imports/api/types"
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
            "Total No. Commits": getTotalCommitsPerContributor(repositoryData, contributor.value.name),
            "LOC": getLOCperContributor(repositoryData, contributor.value.name),
            "LOC Per Commit": getLocPerCommitPerContributor(repositoryData, contributor.value.name),
            "Commits Per Day": getCommitPerDayPerContributor(repositoryData, contributor.value.name),
        },
    })); 
}

/** 
 * This function provides the percentile of a contributors metric.
 */
export function getContributorScaledMetric(data: FilteredData, selectedMetric: MetricType): ContributorScaledMetric {
 // things happen here 
}

export function getContributorScaledData(data: FilteredData, selectedMetric:MetricType): ContributorScaledData[]{
    // things happen here 
    // this would iterate through each contributor and getContributorScaledMetric for each contributor
}