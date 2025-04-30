import React, {FC} from "react";
import {Card, CardContent, CardHeader, CardTitle} from "@ui/components/ui/card";
import LoginForm from "@ui/components/widgets/login/LoginForm/LoginForm";

export interface LoginWidgetProps {
  className?: string
}


const LoginWidget: FC<LoginWidgetProps> = (props) => {

  const cardElement = (
    <Card className="max-w-md">
      <CardHeader>
        <CardTitle>
          Log in
        </CardTitle>
      </CardHeader>
      <CardContent>
        <LoginForm/>
      </CardContent>
    </Card>
  );

  return cardElement;

  return (
    <div className="flex flex-row content-center justify-center">
      {cardElement}
    </div>
  );
}

export default LoginWidget;