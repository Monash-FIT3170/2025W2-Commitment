import NotAuthorizedView from "@ui/views/NotAuthorizedView/NotAuthorizedView";
import React, {ReactNode, useMemo} from "react";
import {useTracker} from "meteor/react-meteor-data";
import {Meteor} from "meteor/meteor";


export interface AuthorizedProps {
  predicate?: (user: Meteor.User) => void,
  children?: ReactNode,
}

/**
 * A helper for routes that swaps out a page that needs a user to be authenticated to access to a NotAuthorizedView when
 * the user is not logged in.
 * @constructor
 */
export default function Authorized(props: AuthorizedProps) {
  const { predicate, children } = props;

  const user = useTracker(() => Meteor.user());
  const isLoggedIn = useMemo<boolean>(() => (
    user !== null && user !== undefined
  ), [user]);
  const isAuthorized = useMemo<boolean>(() => (
    isLoggedIn && (predicate?.(user!) ?? isLoggedIn)
  ), [isLoggedIn, user]);

  return isAuthorized ? <>{children}</> : <NotAuthorizedView />;
}