import React from "react";
import { useFind, useSubscribe } from "meteor/react-meteor-data";
import { LinksCollection, Link } from "../../../../api/bookmarks";
import {Card, CardContent, CardHeader, CardTitle} from "@ui/components/ui/card";
import {Button} from "@ui/components/ui/button";

export const Info = () => {
  const isLoading = useSubscribe("links");
  const links = useFind(() => LinksCollection.find());

  if (isLoading()) {
    return <div>Loading...</div>;
  }

  const makeLink = (link: Link) => {
    return (
      <li key={ link._id }>
        <Button variant="link" onClick={()=> window.open(link.url, "_blank")}>{ link.title }</Button>
      </li>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle>Learn Meteor!</CardTitle>
      </CardHeader>
      <CardContent className="flex flex-col gap-3 content-center">
        <div>
          <ul>{ links.map(makeLink) }</ul>
        </div>
      </CardContent>
    </Card>
  );
};
