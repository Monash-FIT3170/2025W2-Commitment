import React, {FC} from "react";
import {Button} from "@ui/components/ui/button";
import { z } from "zod";
import {zodResolver} from "@hookform/resolvers/zod";
import {useForm} from "react-hook-form";
import {Form, FormControl, FormField, FormItem, FormLabel} from "@ui/components/ui/form";
import {Checkbox} from "@ui/components/ui/checkbox";
import FormInputWithErrors from "../../../shared/FormInputWithErrors";
import { useNavigate } from 'react-router-dom';
import { Meteor } from 'meteor/meteor';
import LoginFormErrorMessage from "@ui/components/widgets/login/LoginForm/LoginFormErrorMessage";


export interface LoginFormProps {
  className?: string
}


const formSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8),
  rememberMe: z.boolean()
})


const LoginForm: FC<LoginFormProps> = (props) => {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      email: "",
      password: "",
      rememberMe: false
    },
  });

  const navigate = useNavigate();

  function onSubmit(values: z.infer<typeof formSchema>) {
    form.clearErrors();

    Meteor.loginWithPassword(values.email, values.password, (err: any) => {
      if (err) {
        // Handle different types of login errors
        let errorMessage = "Login failed. Please check your credentials.";
        
        if (err.reason === 'User not found') {
          errorMessage = "No account found with this email address.";
        } else if (err.reason === 'Incorrect password') {
          errorMessage = "Incorrect password. Please try again.";
        } else if (err.reason === 'User has no password set') {
          errorMessage = "Please reset your password to continue.";
        }

        form.setError("email", { 
          type: "manual", 
          message: errorMessage 
        });
        
        // Also set error on password field for better UX
        form.setError("password", { 
          type: "manual", 
          message: " " // Empty space to maintain form layout
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
      <form onSubmit={form.handleSubmit(onSubmit)} className={`flex flex-col gap-2 ${props.className ?? ""}`}>
        <FormField
          control={form.control}
          name="email"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <FormInputWithErrors placeholder="Email" {...field} type="email"/>
              </FormControl>
              <LoginFormErrorMessage/>
            </FormItem>
          )}
        />

        <FormField
          control={form.control}
          name="password"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <FormInputWithErrors placeholder="Password" type="password" {...field}/>
              </FormControl>
              <LoginFormErrorMessage/>
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
                    <Checkbox checked={field.value} onCheckedChange={field.onChange}/>
                  </FormControl>
                  <div className="space-y-1 leading-none align-middle inline-flex flex-col content-center">
                    <FormLabel>
                      Remember me?
                    </FormLabel>
                  </div>
                </div>
              </FormItem>
            )}
          />
        </div>

        <Button type="submit" className="w-full">Submit</Button>
      </form>
    </Form>
  );
}

export default LoginForm;