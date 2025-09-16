import NotAuthorizedView from "@ui/views/NotAuthorizedView/NotAuthorizedView";
import React, {ReactNode} from "react";
import {Meteor} from "meteor/meteor";
import { useAuth } from "../../hooks/useAuth";


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

  const isAuthorized = useAuth(predicate);


  return isAuthorized ? <>{children}</> : <NotAuthorizedView />;
}