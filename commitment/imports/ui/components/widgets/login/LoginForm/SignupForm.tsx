import React, { FC } from "react";
import { Button } from "@ui/components/ui/button";
import { z } from "zod";
import { zodResolver } from "@hookform/resolvers/zod";
import { useForm } from "react-hook-form";
import { Form, FormControl, FormField, FormItem } from "@ui/components/ui/form";
import { useNavigate } from "react-router-dom";
import { Meteor } from "meteor/meteor";
import LoginFormErrorMessage from "@ui/components/widgets/login/LoginForm/LoginFormErrorMessage";
import { Accounts } from "meteor/accounts-base";
import FormInputWithErrors from "../../../shared/FormInputWithErrors";
import { Eye, EyeOff } from "lucide-react";

export interface SignupFormProps {
  className?: string;
}

const formSchema = z.object({
  username: z
    .string()
    .min(2)
    .max(50)
    .regex(
      /^([a-zA-Z0-9]|[_\-+=|$])+$/,
      "Username can only contain numbers, letters and characters _-+=|$"
    ),
  email: z.string().email(),
  password: z.string().min(8),
  confirmPassword: z.string(),
});

const SignupForm: FC<SignupFormProps> = (props) => {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      username: "",
      email: "",
      password: "",
      confirmPassword: "",
      // rememberMe: false
    },
  });

  const navigate = useNavigate();

  const [showPassword, setShowPassword] = React.useState(false);
  const [showRetypePassword, setShowRetypePassword] = React.useState(false);

  function onSubmit(values: z.infer<typeof formSchema>) {
    form.clearErrors();

    // Ensure passwords match
    if (values.password !== values.confirmPassword) {
      form.setError("password", {
        type: "manual",
        message: " ",
      });

      form.setError("confirmPassword", {
        type: "manual",
        message: "Passwords do not match. Please retype your password.",
      });
      return;
    }

    Accounts.createUser(
      {
        username: values.username,
        email: values.email,
        password: values.password,
        profile: {
          name: values.username,
        },
      },
      (err?: Meteor.Error) => {
        if (err) {
          // Handle different types of login errors
          let errorMessage = `Signup failed: ${err.reason}`;

          if (err.reason === "User not found") {
            errorMessage = "No account found with this email address.";
          } else if (err.reason === "Incorrect password") {
            errorMessage = "Incorrect password. Please try again.";
          } else if (err.reason === "User has no password set") {
            errorMessage = "Please reset your password to continue.";
          }

          form.setError("username", {
            type: "manual",
            message: errorMessage,
          });

          // Also set error on password field for better UX
          // form.setError("name", {
          //   type: "manual",
          //   message: " " // Empty space to maintain form layout
          // });
          form.setError("email", {
            type: "manual",
            message: " ", // Empty space to maintain form layout
          });
          form.setError("password", {
            type: "manual",
            message: " ", // Empty space to maintain form layout
          });
          form.setError("confirmPassword", {
            type: "manual",
            message: " ", // Empty space to maintain form layout
          });
        } else {
          navigate("/home");
        }
      }
    );
  }

  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className={`flex flex-col gap-2 ${props.className ?? ""}`}
      >
        <FormField
          control={form.control}
          name="username"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <FormInputWithErrors
                  placeholder="Username"
                  {...field}
                  type="username"
                />
              </FormControl>
              <LoginFormErrorMessage />
            </FormItem>
          )}
        />

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

        <FormField
          control={form.control}
          name="password"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <div className="relative">
                  <FormInputWithErrors
                    {...field}
                    type={showRetypePassword ? "text" : "password"}
                    placeholder="Retype Password"
                    className="pr-10"
                  />
                  <button
                    type="button"
                    onClick={() => setShowRetypePassword((prev) => !prev)}
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

        <Button type="submit" className="w-full">
          Submit
        </Button>
      </form>
    </Form>
  );
};

export default SignupForm;
