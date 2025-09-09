import { Meteor } from 'meteor/meteor'

/**
 * A method which properly awaits a meteor method execution, which guarantees in-order operation
 * @param method the method to call
 * @param args arguments to give the meteor method 
 * @returns the value which the meteor method will return (templated for convenience)
 */
export const executeMeteorMethod = <T = any>(method: string) => (...args: any[]): Promise<T> => 
  new Promise((resolve, reject) => 
    Meteor.callAsync(method, ...args, (err: Error, res: T) => {
      if (err) reject(err)
      resolve(res)
    })
  ) 