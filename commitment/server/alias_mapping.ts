import { AliasConfig, StudentAlias, AliasConfigsCollection } from "../imports/api/alias_configs";
import { SerializableRepoData, ContributorData } from "../imports/api/types";

/**
 * CORE ALIAS MAPPING ENGINE
 * 
 * This module provides the core functionality for mapping multiple Git identities
 * to a single official name for accurate student contribution tracking.
 */

/**
 * Creates a mapping from Git identifiers (usernames + emails) to official names
 * @param aliases Array of student alias configurations
 * @returns Map where key is git identifier, value is official name
 */
export function createAliasMapping(aliases: StudentAlias[]): Map<string, string> {
  const mapping = new Map<string, string>();
  
  aliases.forEach(alias => {
    // Map all git usernames to official name
    alias.gitUsernames.forEach(username => {
      mapping.set(username.toLowerCase(), alias.officialName);
    });
    
    // Map all emails to official name
    alias.emails.forEach(email => {
      mapping.set(email.toLowerCase(), alias.officialName);
    });
  });
  
  return mapping;
}

/**
 * Applies alias mapping to repository data, consolidating multiple identities
 * @param data Original repository data
 * @param mapping Alias mapping from createAliasMapping
 * @returns New repository data with aliases applied
 */
export function applyAliasMapping(
  data: SerializableRepoData, 
  mapping: Map<string, string>
): SerializableRepoData {
  // Create a reverse mapping to track which official names we've seen
  const officialNames = new Set<string>();
  mapping.forEach(officialName => officialNames.add(officialName));
  
  // Process commits - update contributor names
  const mappedCommits = (data.allCommits || []).map(commitObj => {
    const commit = commitObj.value;
    const originalName = commit.contributorName;
    const mappedName = mapping.get(originalName.toLowerCase()) || originalName;
    
    return {
      ...commitObj,
      value: {
        ...commit,
        contributorName: mappedName
      }
    };
  });
  
  // Process contributors - consolidate and update
  const contributorMap = new Map<string, ContributorData>();
  
  (data.contributors || []).forEach(contributorObj => {
    const contributor = contributorObj.value;
    const originalName = contributor.name;
    const mappedName = mapping.get(originalName.toLowerCase()) || originalName;
    
    if (contributorMap.has(mappedName)) {
      // Merge with existing contributor
      const existing = contributorMap.get(mappedName)!;
      const mergedEmails = [...new Set([...existing.emails, ...contributor.emails])];
      
      contributorMap.set(mappedName, {
        ...existing,
        emails: mergedEmails
      });
    } else {
      // Create new contributor entry
      contributorMap.set(mappedName, {
        ...contributor,
        name: mappedName
      });
    }
  });
  
  // Convert contributor map back to array format
  const mappedContributors = Array.from(contributorMap.entries()).map(([name, data]) => ({
    key: name,
    value: data
  }));
  
  return {
    ...data,
    allCommits: mappedCommits,
    contributors: mappedContributors
  };
}

/**
 * Retrieves the active alias configuration for a user
 * @param userId Meteor user ID
 * @returns Promise resolving to AliasConfig or null if none exists
 */
export async function getUserAliasConfig(userId: string): Promise<AliasConfig | null> {
  if (!userId) return null;
  
  try {
    const config = await AliasConfigsCollection.findOneAsync({ ownerId: userId });
    return config || null;
  } catch (error) {
    console.error('Error fetching alias config for user:', userId, error);
    return null;
  }
}

/**
 * Checks if a user has any alias configuration
 * @param userId Meteor user ID
 * @returns Promise resolving to boolean
 */
export async function userHasAliasConfig(userId: string): Promise<boolean> {
  if (!userId) return false;
  
  try {
    const count = await AliasConfigsCollection.find({ ownerId: userId }).countAsync();
    return count > 0;
  } catch (error) {
    console.error('Error checking alias config for user:', userId, error);
    return false;
  }
}

/**
 * Applies alias mapping conditionally - only if config exists and has relevant users
 * @param data Repository data to potentially map
 * @param userId User ID to check for config
 * @returns Promise resolving to mapped data or original data
 */
export async function applyAliasMappingIfNeeded(
  data: SerializableRepoData, 
  userId: string
): Promise<SerializableRepoData> {
  // Quick check if user has any config
  const hasConfig = await userHasAliasConfig(userId);
  
  if (!hasConfig) {
    return data;
  }
  
  // Get the actual config
  const config = await getUserAliasConfig(userId);
  
  if (!config) {
    return data;
  }
  
  // Check if any contributors match the config
  const hasRelevantUsers = (data.contributors || []).some(contributor => {
    const name = contributor.value.name.toLowerCase();
    const emails = contributor.value.emails.map(e => e.toLowerCase());
    
    return config.aliases.some(alias => 
      alias.gitUsernames.some(username => username.toLowerCase() === name) ||
      alias.emails.some(email => emails.includes(email.toLowerCase()))
    );
  });
  
  if (!hasRelevantUsers) {
    return data;
  }
  
  // Apply the mapping
  const mapping = createAliasMapping(config.aliases);
  const mappedData = applyAliasMapping(data, mapping);
  
  return mappedData;
}

/**
 * Validates that an alias configuration is properly structured
 * @param config Alias configuration to validate
 * @returns Object with isValid boolean and error message if invalid
 */
export function validateAliasConfig(config: AliasConfig): { isValid: boolean; error?: string } {
  if (!config.aliases || !Array.isArray(config.aliases)) {
    return { isValid: false, error: "Aliases must be an array" };
  }
  
  if (config.aliases.length === 0) {
    return { isValid: false, error: "At least one alias must be provided" };
  }
  
  for (const alias of config.aliases) {
    if (!alias.officialName || typeof alias.officialName !== 'string') {
      return { isValid: false, error: "Each alias must have a valid officialName" };
    }
    
    if (!alias.gitUsernames || !Array.isArray(alias.gitUsernames)) {
      return { isValid: false, error: "Each alias must have gitUsernames array" };
    }
    
    if (!alias.emails || !Array.isArray(alias.emails)) {
      return { isValid: false, error: "Each alias must have emails array" };
    }
    
    if (alias.gitUsernames.length === 0 && alias.emails.length === 0) {
      return { isValid: false, error: "Each alias must have at least one gitUsername or email" };
    }
  }
  
  return { isValid: true };
}
