import React, { FC } from 'react';
import {
  Card, CardContent, CardHeader, CardTitle,
} from '@ui/components/ui/card';
import LoginForm from '@ui/components/widgets/login/LoginForm/LoginForm';
import { cx } from 'class-variance-authority';

export interface LoginWidgetProps {
  className?: string
}

const LoginWidget: FC<LoginWidgetProps> = (className) => {
  const cardElement = (
    <Card className="max-w-md w-96 grow">
      <CardHeader>
        <CardTitle>
          Log in
        </CardTitle>
      </CardHeader>
      <CardContent>
        <LoginForm />
      </CardContent>
    </Card>
  );

  return (
    <div className={cx('inline-flex flex-row content-center justify-center', className)}>
      <div>
        {cardElement}
      </div>
    </div>
  );
};

export default LoginWidget;
