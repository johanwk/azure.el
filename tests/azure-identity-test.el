;;; azure-identity-test.el --- Tests for Azure identities -*- lexical-binding: t; -*-

(require 'ert)
(require 'azure)

(ert-deftest azure-current-user-assignment-prefers-unique-name ()
  (should
   (equal
    (azure--current-user-assignment
     '((providerDisplayName . "Johan Wilhelm Kluewer")
       (uniqueName . "Johan.Wilhelm.Kluewer@example.com")))
    "Johan.Wilhelm.Kluewer@example.com")))

(ert-deftest azure-current-user-assignment-reads-account-property ()
  (should
   (equal
    (azure--current-user-assignment
     '((providerDisplayName . "Johan Wilhelm Kluewer")
       (properties
        (Account ($type . "System.String")
                 ($value . "Johan.Wilhelm.Kluewer@example.com")))))
    "Johan.Wilhelm.Kluewer@example.com")))

(provide 'azure-identity-test)
;;; azure-identity-test.el ends here
