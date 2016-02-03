;; Copyright (C) 2016 Carlos Neira, cneirabustos@gmail.com

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This library implements the current protocol specification documented in
;; https://github.com/brooksc/mcpipy.git
;; This runs on ccl,sbcl and clisp common lisp implementations.

;;According to protocol documentation, commands are just ascii LF terminated sent to the 4711 port of the current host running an instance of minecraft pi.

;; universal sockets for portability between clisp implementations.

(ql:quickload "usocket")

;(use-package  :usocket)

; open the connection

(defparameter *pi-host*  "127.0.0.1" )


(defun send-command (cmd host port)
  (usocket:with-client-socket (usocket:socket stream host port )
    (format stream  "~A~C" cmd #\Newline)
    (force-output stream)
    (read-line stream nil) ))


(defmacro send-to-mcpi (cmd )  
  ` (progn
      (format t "~a~%" ,cmd)
      (send-command ,cmd *pi-host* 4711) ) )


;; This macro creates a function with n-parameters as input
;; intern enters a symbol named string into package.
;; symbol-package Returns the home package of symbol.
;; So with this we could register any function that is in the API or new ones
;; as long as the format is <class>.<method> (<param list>)
;; with :invert  any symbol in CamelCase is preserved like it is originaly. This is needed as
;; the API has CamelCase methods


(defmacro add-mpci-call (name (&rest args) )
  (setf (readtable-case *readtable*) :invert)
  (let*  ((package (symbol-package name) )
	  (func-name (intern (format nil "~a" name) package)))
    `(defun ,func-name ,args
       (send-to-mcpi   (format nil "~a( ~{~a~^, ~} )" ',func-name (list ,@args )) ))) )


;To register a new api method that takes 3 parameters just  call it like this 
;(add-mpci-call  world.getBlock  ( x y z ) )


;; API implementation register all methods
;COMMANDS
;-- World --
;world.getBlock(x,y,z) --> blockTypeId
(add-mpci-call  world.getBlock  ( x y z ) )

;;world.setBlock(x,y,z,blockTypeId)
;; block types ids from here http://minecraft.gamepedia.com/Data_values_(Pocket_Edition)
(add-mpci-call  world.setBlock  ( x y z blockTypeId ) )

 ;;world.setBlock(x,y,z,blockTypeId,blockData)
(add-mpci-call  world.setBlock  ( x y z blockTypeId blockData ) )

;;world.setBlocks(x1,y1,z1,x2,y2,z2,blockTypeId)
(add-mpci-call  world.setBlocks  ( x1 y1 z1 x2 y2 z2 blockTypeId ) )

;;world.setBlocks(x1,y1,z1,x2,y2,z2,blockTypeId,blockData)
(add-mpci-call  world.setBlocks  ( x1 y1 z1 x2 y2 z2 blockTypeId blockData ) )

;;world.getHeight(x,z) --> Integer
(add-mpci-call  world.getHeight  ( x  z ) )

;;world.checkpoint.save()
(add-mpci-call  world.checkpoint.save (  ) )

;;world.checkpoint.restore()
(add-mpci-call  world.checkpoint.restore (  ) )

;;chat.post(message)
(add-mpci-call  chat.post (message ) )


;;-- Camera --

;;camera.mode.setNormal()
(add-mpci-call  camera.mode.setNormal (  ) )
;;camera.mode.setThirdPerson()
(add-mpci-call  camera.mode.setThirdPerson (  ) )
;;camera.mode.setFixed()
(add-mpci-call  camera.mode.setFixed (  ) )

;;camera.mode.setPos(x,y,z)
(add-mpci-call  camera.mode.setPos (  ) )

;;;-- Player --
;player.getTile() --> x,y,z
(add-mpci-call  player.getTile (  ) )
;;test 
;; (player.getTile )

;;player.setTile(x,y,z)
(add-mpci-call  player.setTile (x y z  ))

;;player.getPos() --> xf,yf,zf
(add-mpci-call  player.getPos ( ))

;;player.setPos(xf,yf,zf)
(add-mpci-call  player.setPos (xf yf zf ))

;-- Entities --
;TBD

;-- Events --
;;events.block.hits() --> pos,surface,entityId|pos,surface,entityId|... (pos is x,y,z surface is x,y,z, entityId is int)
(add-mpci-call  events.block.hits ( ))

;;events.clear
(add-mpci-call  events.clear ( ))

;;some tests

;; a lava block is 10 dec
(world.setBlock -68  0 50 10 )
(chat.post "Hello")
