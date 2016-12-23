package com.github.cynic.netcommand

// See: http://www.minecraftforge.net/wiki/Installation/Source
// Helpful:
// - http://stackoverflow.com/questions/14408273/akka-socket-per-actor
// - http://doc.akka.io/docs/akka/2.4.1/scala/io-tcp.html

/*
 * Basic API:
 * 
 * PLAYERPOS : returns the player position as (x,y,z)
 * PLAYERLOOK : returns the direction that the player is
 *              looking as normalized (x,y,z) vector relative to PLAYERPOS
 * GETBLOCK(x,y,z) : returns the blocktype at (x,y,z)
 * SETBLOCK(x,y,z,v) : sets the block at (x,y,z) to type v
 * 
 * ...anything else can be left for later!  e.g.
 * COMMAND(cmd) : execute the command cmd
 * CHAT(txt) : say hello! :D
 */

import net.minecraftforge.fml.common.FMLLog
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.Mod.{EventHandler,Instance}
import net.minecraftforge.fml.common.event.{FMLInitializationEvent,FMLPostInitializationEvent,FMLPreInitializationEvent}
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.fml.common.gameevent.TickEvent.PlayerTickEvent
import java.net.InetSocketAddress
import akka.actor.{Actor,ActorRef,Props}
import akka.io.{IO,Tcp}
import akka.util.ByteString
import scala.collection.JavaConversions._
import scala.util.matching.Regex

import net.minecraft.util.{EnumFacing,ResourceLocation}
import net.minecraft.block.properties.{PropertyBool,PropertyEnum,PropertyDirection,PropertyInteger}
import net.minecraft.util.math.{Vec3d,BlockPos}
import net.minecraft.item.Item
import net.minecraft.block.Block
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayer
import java.util.function.Consumer
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.client.MinecraftForgeClient
import net.minecraftforge.fml.common.Mod.EventBusSubscriber

/*
 * Architecture:
 * 
 * 1. There is a Server actor that listens for connections.
 * 2. There is a NetObject in which a method gets called on every living tick.
 * 3. The Server actor spawns a WorldUpdater actor to update the world.
 * 4. When the Server gets a connection, it spawn a Client actor to handle it.
 * 5. When a Client actor receives a command, it sends it to the WorldUpdater.
 * 6. When the WorldUpdater receives a Client command, it queues it.
 * 7. When the NetObject ticks, it passes the World to WorldUpdater, synchronously.
 * 8. When the WorldUpdater gets the World, it executes all queued Client commands,
 *     sending messages back to the Client as necessary.
 */

abstract class Direction
case class PositiveX() extends Direction
case class NegativeX() extends Direction
case class Up() extends Direction
case class Down() extends Direction
case class PositiveZ() extends Direction
case class NegativeZ() extends Direction

abstract class UserCommand
case class PlayerPos() extends UserCommand // "pos"
case class PlayerLook() extends UserCommand // "look"
case class GetOrigin(dist:Int) extends UserCommand // "origin [n]" (n must be positive)
case class GetBlock (x:Int, y:Int, z:Int) extends UserCommand // "getblock [x] [y] [z]"
case class SetBlock (v:Int, x:Int, y:Int, z:Int, facing:Direction) extends UserCommand // "set [id] [x] [y] [z] [+y|-y|+x|-x|+z|-z]"
case class BlockIDs () extends UserCommand // "idlist"

abstract class WorldCommand
case class UpdateRequest(x:UserCommand) extends WorldCommand
case class WorldTick(w: EntityPlayer) extends WorldCommand

object CommandQueuer {
  import scala.collection.mutable
  val queue = new java.util.concurrent.ConcurrentLinkedQueue[(ActorRef,UserCommand)]
}

class WorldUpdater extends Actor {
  val logger = FMLLog.getLogger()
  val queue = CommandQueuer.queue

  def receive = {
    case UpdateRequest(cmd) =>
      val s = sender ()
      queue.add( (s, cmd) )
    case WorldTick(p) =>
      while (!queue.isEmpty()) {
        queue.remove() match {
          case (s,cmd) => (dispatch(p)) (s,cmd)
        }        
      }
  }
  
  def dispatch (player: EntityPlayer) = (s:ActorRef, x:UserCommand) =>
    x match {
      case PlayerPos () =>
        s ! ByteString ( (player.posX, player.posY, player.posZ).toString() + "\n" )
      case PlayerLook () =>
        val v3d = player.getLookVec()
        s ! ByteString ( (v3d.xCoord, v3d.yCoord, v3d.zCoord).toString() + "\n" )
      case GetOrigin (dist) =>
        val v3d = player.getLookVec()
        val res = new Vec3d (player.posX + v3d.xCoord * dist, player.posY + v3d.yCoord * dist + player.getEyeHeight(), player.posZ + v3d.zCoord * dist)
        s ! ByteString( (res.xCoord, res.yCoord, res.zCoord).toString() + "\n" )
      case GetBlock (x, y, z) =>
        val pos = new BlockPos(x, y, z)
        val blockState = player.world.getBlockState(pos)
        val block = blockState.getBlock()
        s ! ByteString( net.minecraft.block.Block.getIdFromBlock(block).toString() + "\n" )
      case SetBlock (v, x, y, z, facing) =>
        val pos = new BlockPos(x, y, z)
        player.world.destroyBlock(pos, false)
        val block = Block.getBlockById(v)
        val blockState = block.getDefaultState()
        blockState.getPropertyKeys().toList.find(_.isInstanceOf[PropertyDirection]) match {
          case None => // there's no FACING property, so ignore what's passed in.
            player.world.setBlockState(pos, blockState)
          case Some(propKey) => // there is a FACING property, so use what's passed in.
            val dir =
              facing match {
                case Up () => EnumFacing.UP
                case Down () => EnumFacing.DOWN
                case PositiveX () => EnumFacing.EAST
                case NegativeX () => EnumFacing.WEST
                case PositiveZ () => EnumFacing.SOUTH
                case NegativeZ () => EnumFacing.NORTH
              }
            player.world.setBlockState(pos, blockState.withProperty(propKey.asInstanceOf[PropertyDirection], dir))
            //s ! ByteString("OK")
        }
      case BlockIDs () =>
        val name = Block.REGISTRY.getKeys().toList.foreach { f =>
          val dom = f.getResourceDomain ()
          val path = f.getResourcePath ()
          //val rl = new ResourceLocation(dom, path)
          val block = Block.REGISTRY.getObject(f)
          val id = Block.getIdFromBlock(block)
          s ! ByteString( (dom, path, id).toString() )
          // NOTE: Variants are a huge mess.  So I'm basically ignoring them at this point!
        }
        s ! ByteString("\n")
    }
}

class Server extends Actor {
  val logger = FMLLog.getLogger()
  import Tcp._
  import context.system
  IO(Tcp) ! Bind(self, new InetSocketAddress(4321))
  val updater = context.actorSelection("../updater")
  
  def receive = {
    case b @ Bound(localAddress) =>
      logger.info("Bound to " + localAddress.getHostName() + ":" + localAddress.getPort())
    case CommandFailed(_: Bind) =>
      logger.warn("Binding failed!  Mod won't function.")
      context stop self
    case c @ Connected(remote, local) =>
      logger.info("Connected: " + remote.getHostName() + ":" + remote.getPort() + " <=> " + local.getHostName() + ":" + local.getPort())
      val conn = sender ()
      val handler = context.actorOf(Props(classOf[ConnectionHandler], conn))
      conn ! Register(handler)
  }
}

class ConnectionHandler(conn : ActorRef) extends Actor {
  val logger = FMLLog.getLogger()
  val updater = context.actorSelection("../../updater")
  val server = context.actorSelection("../server")
  val origin = """^origin (\d{1,6})""".r
  val getb = """getblock (-?\d{1,5}) (-?\d{1,5}) (-?\d{1,5})""".r
  val setb = """set (\d{1,5}) (-?\d{1,5}) (-?\d{1,5}) (-?\d{1,5}) ([+-])([xyz])""".r
  import Tcp._
  def parse (s:String) : Option[UserCommand] =
    if (s == "pos") { return Some (PlayerPos ()) }
    else if (s == "look") { return Some (PlayerLook ()) }
    else if (s == "idlist") { return Some (BlockIDs ()) }
    else {
      s match {
        case origin(dist) => return Some (GetOrigin (dist.toInt))
        case setb(id, _x, _y, _z, sign, axis) =>
          val (v, x, y, z) = (id.toInt, _x.toInt, _y.toInt, _z.toInt)
          (sign, axis) match {
            case ("+","x") => return Some (SetBlock(v, x, y, z, PositiveX ())) 
            case ("-", "x") => return Some (SetBlock(v, x, y, z, NegativeX ()))
            case ("+","y") => return Some (SetBlock(v, x, y, z, Up ())) 
            case ("-", "y") => return Some (SetBlock(v, x, y, z, Down ()))
            case ("+","z") => return Some (SetBlock(v, x, y, z, PositiveZ ())) 
            case ("-", "z") => return Some (SetBlock(v, x, y, z, NegativeZ ()))            
            case _ => return None
          }
        case getb(_x, _y, _z) =>
          val (x, y, z) = (_x.toInt, _y.toInt, _z.toInt)
          return Some (GetBlock(x, y, z))
        case _ => return None
      }
    }
  
  def receive = {
    case CommandFailed (x) =>
      logger.warn("Command failed")
      logger.warn(x)
    case Received(data) =>
      logger.info("Received data " + data)
      val str = data.decodeString("UTF-8").trim()
      parse(str) match {
        case None => logger.warn("Couldn't decode: '" + str + "'")
        case Some(v) => updater ! UpdateRequest(v)
      }
      //updater ! UpdateRequest (PlayerPos ()) // FOR TESTING PURPOSES ONLY
    case data : ByteString =>
      logger.info("Writing data " + data)
      conn ! Write(data)
    case PeerClosed => context stop self
  }
}

import akka.actor.ActorSystem
import net.minecraftforge.fml.common.FMLCommonHandler
import net.minecraftforge.fml.common.gameevent.TickEvent.PlayerTickEvent
import net.minecraft.util.EnumFacing
import net.minecraft.init.Blocks
import net.minecraftforge.fml.common.registry.GameRegistry

class TickHandler(updater : ActorRef) {
  val logger = FMLLog.getLogger()

  @SubscribeEvent
  def playerTick(e : PlayerTickEvent) {
    updater ! WorldTick(e.player)
  }
}

@Mod(modid = "netcommand.cynic", name = "NetCommand", modLanguage = "scala", version = "0.0.1")
object NetCommand {
  val logger = FMLLog.getLogger()
  val actorSys = ActorSystem.create("netcommand")

  @EventHandler
  def preInit(e: FMLPreInitializationEvent) {
    logger.info("pre-init: creating actor system, adding TickHandler")
    val updater = actorSys.actorOf(Props[WorldUpdater], "updater")
    MinecraftForge.EVENT_BUS.register(new TickHandler(updater))
  }

  @EventHandler
  def init(e: FMLInitializationEvent) {
    logger.info("init: starting server")
    val server = actorSys.actorOf(Props[Server], "server")
  }  

  @EventHandler
  def postInit(e: FMLPostInitializationEvent) {
    logger.info("post-init.")
  }  
}