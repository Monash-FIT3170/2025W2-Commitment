import React from "react";
import { Button } from "@base/button";
import { Checkbox } from "@base/checkbox";
import { z } from "zod";
import { zodResolver } from "@hookform/resolvers/zod";
import { useForm } from "react-hook-form";
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
} from "@base/form";
import { useNavigate } from "react-router-dom";
import { Meteor } from "meteor/meteor";
import { Eye, EyeOff } from "lucide-react";
import LoginFormErrorMessage from "@ui/components/login/LoginForm/LoginFormErrorMessage";

import FormInputWithErrors from "@ui/components/shared/FormInputWithErrors";
import GoogleLoginWidget from "@ui/components/widgets/login/GoogleLoginWidget";
import GithubLoginWidget from "@ui/components/widgets/login/GithubLoginWidget";

export interface LoginFormProps {
  className?: string;
}

const formSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8),
  rememberMe: z.boolean(),
});

function LoginForm(props: LoginFormProps) {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      email: "",
      password: "",
      rememberMe: false,
    },
  });

  const navigate = useNavigate();

  const [showPassword, setShowPassword] = React.useState(false);

  function onSubmit(values: z.infer<typeof formSchema>) {
    form.clearErrors();

    Meteor.loginWithPassword(values.email, values.password, (err: any) => {
      if (err) {
        // Handle different types of login errors
        let errorMessage = "Login failed. Please check your credentials.";

        if (err.reason === "User not found") {
          errorMessage = "No account found with this email address.";
        } else if (err.reason === "Incorrect password") {
          errorMessage = "Incorrect password. Please try again.";
        } else if (err.reason === "User has no password set") {
          errorMessage = "Please reset your password to continue.";
        }

        form.setError("email", {
          type: "manual",
          message: errorMessage,
        });

        // Also set error on password field for better UX
        form.setError("password", {
          type: "manual",
          message: " ", // Empty space to maintain form layout
        });
      } else {
        if (values.rememberMe) {
          // Set a cookie or localStorage item to remember the user
          localStorage.setItem("rememberedUser", values.email);
        } else {
          // Clear the remembered user if not checked
          localStorage.removeItem("rememberedUser");
        }

        navigate("/home");
      }
    });
  }

  return (
    <Form {...form}>
      <form
        onSubmit={(e) => {
          void form.handleSubmit(onSubmit)(e);
        }}
        className={`flex flex-col gap-2 ${props.className ?? ""}`}
      >
        <FormField
          control={form.control}
          name="email"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <FormInputWithErrors
                  placeholder="Email"
                  {...field}
                  type="email"
                />
              </FormControl>
              <LoginFormErrorMessage />
            </FormItem>
          )}
        />

        <FormField
          control={form.control}
          name="password"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <div className="relative">
                  <FormInputWithErrors
                    {...field}
                    type={showPassword ? "text" : "password"}
                    placeholder="Password"
                    className="pr-10"
                  />
                  <button
                    type="button"
                    onClick={() => setShowPassword((prev) => !prev)}
                    className="absolute right-2 top-1/2 -translate-y-1/2 text-muted-foreground focus:outline-none"
                    tabIndex={-1}
                  >
                    {showPassword ? (
                      <EyeOff className="h-4 w-4" />
                    ) : (
                      <Eye className="h-4 w-4" />
                    )}
                  </button>
                </div>
              </FormControl>
              <LoginFormErrorMessage />
            </FormItem>
          )}
        />

        <div className="inline-flex flex-col justify-center h-9">
          <FormField
            control={form.control}
            name="rememberMe"
            render={({ field }) => (
              <FormItem>
                <div className="flex items-center space-x-2 ml-3">
                  <FormControl>
                    <Checkbox
                      checked={field.value}
                      onCheckedChange={field.onChange}
                    />
                  </FormControl>
                  <div className="space-y-1 leading-none align-middle inline-flex flex-col content-center">
                    <FormLabel>Remember me?</FormLabel>
                  </div>
                </div>
              </FormItem>
            )}
          />
        </div>

        <Button type="submit" className="w-full">
          Submit
        </Button>

        <div className="relative my-4">
          <div className="absolute inset-0 flex items-center">
            <span className="w-full border-t" />
          </div>
          <div className="relative flex justify-center text-xs uppercase">
            <span className="bg-git-bg-tertiary px-2 text-muted-foreground">
              Or instead
            </span>
          </div>
        </div>

        <div className="flex flex-col gap-2">
          <GoogleLoginWidget />
          <GithubLoginWidget />
        </div>
      </form>
    </Form>
  );
}

export default LoginForm;
