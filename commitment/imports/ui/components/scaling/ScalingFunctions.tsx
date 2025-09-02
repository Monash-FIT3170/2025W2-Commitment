import { RepositoryData, CommitData, UserScalingSummary } from "/imports/api/types";
import { GradingSheetParserService, type ParseResult, type GradingSheetRow } from "../utils/GradingSheetParser";

// All possible metrics (same as Python)
const USER_DATA_METRICS = [
  "Total No. Commits",
  "LOC",
  "LOC per commit",
  "Commits per day",
];

function normalizeMetric(values: number[], alpha = 0.2): number[] {
  //alpha determines leniancy - smooths out values

  const minVal = Math.min(...values);
  const maxVal = Math.max(...values);
  const rangeVal = maxVal !== minVal ? maxVal - minVal : 1;

  const normalized = values.map((v) => (v - minVal) / rangeVal);
  return normalized.map((n) => alpha + (1 - 2 * alpha) * n);
}

export function scaleUsers(
  repoData: RepositoryData,
  config: { metrics: string[]; method: string }
) {
  const selectedMetrics = config.metrics ?? [];
  const method = config.method ?? "Percentiles";

  const userNames = Array.from(
    new Set(
      Array.from(repoData.allCommits.values()).map((c) => c.contributorName)
    )
  );

  const users: [string, number[]][] = userNames.map((name) => {
    const commits = Array.from(repoData.allCommits.values()).filter(
      (c) => c.contributorName === name
    );

    const values = USER_DATA_METRICS.map((metric) => {
      switch (metric) {
        case "Total No. Commits":
          return commits.length;
        case "LOC":
          return commits.reduce((sum, c) => sum + getLOCFromCommit(c), 0);
        case "LOC per commit":
          return commits.length
            ? commits.reduce((sum, c) => sum + getLOCFromCommit(c), 0) /
                commits.length
            : 0;
        case "Commits per day":
          if (commits.length === 0) return 0;
          const timestamps = commits.map((c) => c.timestamp.getTime());
          const days =
            (Math.max(...timestamps) - Math.min(...timestamps)) /
              (1000 * 60 * 60 * 24) || 1;
          return commits.length / days;
        default:
          return 0;
      }
    });

    return [name, values];
  });
  console.log(
    "Metric values for Amy:",
    users.map((u) => u[1])
  );

  const metricIndices = selectedMetrics
    .map((m) => USER_DATA_METRICS.indexOf(m))
    .filter((idx) => idx >= 0);

  if (metricIndices.length === 0) {
    return users.map(([name]) => ({ name, score: 0.0 }));
  }

  const metricsValues: number[][] = metricIndices.map((idx) =>
    normalizeMetric(users.map((u) => u[1][idx]))
  );

  return users.map(([name], i) => {
    const userScores = metricsValues.map((mv) => mv[i]);
    console.log(`User: ${name}, metric scores:`, userScores);

    let score: number;
    if (method === "Percentiles") {
      score = userScores.reduce((a, b) => a + b, 0) / userScores.length;
    } else if (method === "Mean +/- Std") {
      const mean = userScores.reduce((a, b) => a + b, 0) / userScores.length;
      const std = Math.sqrt(
        userScores.reduce((sum, x) => sum + (x - mean) ** 2, 0) /
          userScores.length
      );
      score = mean + std; // do not clamp between 0-1 yet
    } else if (method === "Quartiles") {
      const sorted = [...userScores].sort((a, b) => a - b);
      const mid = Math.floor(sorted.length / 2);
      score =
        sorted.length % 2 === 0
          ? (sorted[mid - 1] + sorted[mid]) / 2
          : sorted[mid];
    } else {
      score = userScores.reduce((a, b) => a + b, 0) / userScores.length;
    }

    console.log("Selected metrics indices:", metricIndices);
    console.log("Normalized metrics values:", metricsValues);

    // Round to 2 decimals -> never round down to 0
    console.log(`Final score for ${name}:`, score);

    return { name, score: Math.round(score * 100) / 100 };
  });
}

// Helper to calculate LOC from a commit -> ideally we dont do this
function getLOCFromCommit(commit: CommitData): number {
  if (!commit.fileData) return 0;
  return commit.fileData.reduce((acc, fileChange) => {
    const content = fileChange.file?.contents || "";
    return acc + content.split("\n").length;
  }, 0);
}

// Function to calculate final grades by matching contributors with grading sheet data
export function calculateFinalGrades(scalingResults: UserScalingSummary[], gradingData: GradingSheetRow[]): UserScalingSummary[] {
  return scalingResults.map(contributor => {
    const matchingStudent = gradingData.find(student => {
      const studentName = student.fullName.toLowerCase().trim();
      const contributorName = contributor.name.toLowerCase().trim();
      const studentEmail = student.emailAddress.toLowerCase().trim();
      if (studentName === contributorName) {
        return true;
      }
      return contributor.aliases.some(alias => 
        alias.email.toLowerCase().trim() === studentEmail
      );
    });

    if (matchingStudent) {
      const percentageGrade = (matchingStudent.grade / matchingStudent.maximumGrade) * 100;
      const finalGrade = percentageGrade * contributor.scale;        
      return {
        ...contributor,
        finalGrade: Math.round(finalGrade * 100) / 100
      };
    }
    
    return {
      ...contributor,
      finalGrade: null
    };
  });
}

// Helper function to generate updated grading sheet CSV with scaled grades
export async function generateScaledGradingSheet(
  originalParseResult: ParseResult,
  scalingResults: UserScalingSummary[]
): Promise<File> {
  return GradingSheetParserService.unParseGradingSheet(originalParseResult, scalingResults);
}
