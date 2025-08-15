import React, {FC, ReactNode, useCallback} from "react";
import {Card, CardContent, CardHeader, CardTitle} from "@ui/components/ui/card";
import LoginForm from "@ui/components/widgets/login/LoginForm/LoginForm";
import {cx} from "class-variance-authority";
import SignupForm from "@ui/components/widgets/login/LoginForm/SignupForm";
import {Tabs, TabsContent, TabsList, TabsTrigger} from "@ui/components/ui/tabs";

export interface LoginWidgetProps {
  className?: string,
  defaultTab: "login" | "signup"
}

interface Page {
  name: string,
  title: string,
  form: ReactNode,
}

const pages: Page[] = [
  {
    name: "login",
    title: "Log in",
    form: (<LoginForm/>)
  },
  {
    name: "signup",
    title: "Sign up",
    form: (<SignupForm/>)
  }
];

const LoginWidget: FC<LoginWidgetProps> = (props) => {

  const CreateCardElement = useCallback((page: Page) => (
    <TabsContent value={page.name} key={page.name}>
      <Card className="max-w-md w-96 grow">
        <CardHeader>
          <CardTitle>
            {page.title}
          </CardTitle>
        </CardHeader>
        <CardContent>
          {page.form}
        </CardContent>
      </Card>
    </TabsContent>
  ), []);

  return (
    <div className={cx("inline-flex flex-row content-center justify-center", props.className ?? "")}>
      <Tabs defaultValue={props.defaultTab}>
        <TabsList>
          {pages.map((page: Page, i: number) => (
            <TabsTrigger value={page.name} key={page.name}>{page.title}</TabsTrigger>
          ))}
        </TabsList>
        {pages.map(CreateCardElement)}
      </Tabs>
    </div>
  );
}

export default LoginWidget;