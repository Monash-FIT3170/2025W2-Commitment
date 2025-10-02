import React, { FC, ReactNode, useCallback } from 'react';
import {
  Card, CardContent, CardHeader, CardTitle,
} from '@base/card';
import LoginForm from '/imports/ui/components/login/LoginForm/LoginForm';
import SignupForm from "/imports/ui/components/login/LoginForm/SignupForm";
import { cx } from 'class-variance-authority';
import {Tabs, TabsContent, TabsList, TabsTrigger} from "@base/tabs";

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
    <TabsContent value={page.name}>
      <Card className="max-w-md w-96 grow border-border bg-git-bg-tertiary stroke-0 rounded-none rounded-ee-xl rounded-es-xl shadow-none">
        <CardContent className="pt-3">
          {page.form}
        </CardContent>
      </Card>
    </TabsContent>
  ), []);

  return (
    <div className={cx('inline-flex flex-row content-center justify-center', props.className)}>
      <Tabs defaultValue={props.defaultTab}>
        <TabsList className='bg-git-bg-tertiary rounded-ss-xl rounded-se-xl pt-2'>
          {pages.map((page: Page) => (
            <TabsTrigger value={page.name}>{page.title}</TabsTrigger>
          ))}
        </TabsList>
        <div className='h-96'>
          {pages.map(CreateCardElement)}
        </div>
      </Tabs>
    </div>
  );
};

export default LoginWidget;
