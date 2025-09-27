import React, {ReactNode, useMemo} from "react";
import {useTracker} from "meteor/react-meteor-data";
import { Meteor } from "meteor/meteor";

// Hook to verify if user is signed in
export function useAuth(  predicate?: (user: Meteor.User) => void
) {

  const user = useTracker(() => Meteor.user());
  const isLoggedIn = useMemo<boolean>(() => (
    user !== null && user !== undefined
  ), [user]);
  const isAuthorized = useMemo<boolean>(() => (
    isLoggedIn && (predicate?.(user!) ?? isLoggedIn)
  ), [isLoggedIn, user]);
    
    return isAuthorized
}