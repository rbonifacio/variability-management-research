package br.unb.cdt.desafioPositivo.facade;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.persistence.EntityManager;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.faces.Renderer;

import br.unb.cdt.desafioPositivo.model.Proposta;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.model.acesso.AcessoSolicitado;
import br.unb.cdt.desafioPositivo.model.acesso.ExcecaoAcessoUsuario;
import br.unb.cdt.desafioPositivo.util.criptografia.CriptografiaUtil;
import br.unb.cdt.desafioPositivo.util.rest.AutenticacaoSRV;
import br.unb.cdt.desafioPositivo.util.rest.CadastroSRV;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaAutenticacao;
import br.unb.cdt.desafioPositivo.util.rest.CodigoRespostaCadastro;
import br.unb.cdt.desafioPositivo.util.rest.RespostaPositivo;

@Name("facade")
@AutoCreate
/**
 * Fachada da aplicacao, disponibilizando os metodos 
 * transacionais que permitem cadastrar usuarios e enviar propostas.
 * 
 * @author positivo
 */
public class DesafioPositivoFacade {

	private static final String EMAIL_CADASTRO_USUARIO_XHTML = "/email/cadastroUsuario.xhtml";

	@In
	private EntityManager entityManager;
	
	@In(create=true)
	private Renderer renderer;

	/**
	 * Adiciona um usuario no meio de persistencia e realiza uma requisicao ao
	 * servico correspondente da positivo.
	 * 
	 * @param dto
	 *            usuario a ser cadastrado
	 * @throws Exception
	 *             Caso algum problema tenha ocorrido.
	 */
	public void adicionarUsuario(Usuario dto) throws ExcecaoUsuarioCadastrado, ExcecaoEnvioEmail, Exception {
		AutenticacaoSRV autentica = new AutenticacaoSRV(dto.getEmail(), "123456");
		
		autentica.preparaRequisicao();
		int resp = autentica.requisitaServico().getCodigo();
		
		switch(CodigoRespostaAutenticacao.fromCodigo(resp)) {
		  case CLIENTE_NAO_ENCONTRADO:  
			cadastraNovoUsuario(dto); 
			break; 
		
		  case SENHA_INVALIDA: throw new ExcecaoUsuarioCadastrado();
		
		  case SUCESSO: throw new ExcecaoUsuarioCadastrado();
		  
		  default: throw new Exception("Nao foi possivel efetuar o cadastro."); 
		}
	}

	/**
	 * Confirma a solicitacao de cadastro.
	 * @param dto dados da confirmacao de solicitacao de cadastro
	 */
	public void confirmarSolicitacaoCadstro(Usuario dto) throws ExcecaoUsuarioNaoEncontrado, Exception {
		Usuario usuario = recuperaUsuario(dto.getEmail());
		
		if(usuario != null) {
			usuario.setSenha(dto.getSenha());
			
			CadastroSRV srv = new CadastroSRV(usuario);
			srv.preparaRequisicao();
			RespostaPositivo resp = srv.requisitaServico();
			
			switch(CodigoRespostaCadastro.fromCodigo(resp.getCodigo())) {
		  	  case SUCESSO : 
		  		confirmaCadastro(dto, usuario, resp);
				break;
				
			  case CLIENTE_JA_EXISTE: 
				  //existe na positivo, mas continua pendente aqui
				  if(usuario.getSituacaoAcessoAtual().getClass().equals(AcessoSolicitado.class)) {
					  confirmaCadastro(dto, usuario, resp);
				  }
				  else {
					  throw new ExcecaoUsuarioCadastrado();
				  }
		   	  
			  default:  throw new Exception("Nao foi possivel confirmar o cadastro do usuario.");
			}
			
		}
		else {
			throw new ExcecaoUsuarioNaoEncontrado();
		}
	}

	private void confirmaCadastro(Usuario dto, Usuario usuario,
			RespostaPositivo resp) throws ExcecaoAcessoUsuario {
		usuario.setToken(resp.getToken());
		usuario.getSituacaoAcessoAtual().confirmarCadastro(dto.getCodigoConfirmacaoCadastro());
		entityManager.merge(usuario);
		entityManager.flush();
	}
	/*
	 * Persiste um novo usuario na base de dados.
	 */
	private void cadastraNovoUsuario(Usuario usuario) throws ExcecaoEnvioEmail, Exception {
//		try {
//			renderer.render(EMAIL_CADASTRO_USUARIO_XHTML);
//		}
//		catch(Exception e) {
//			throw new ExcecaoEnvioEmail("Nao foi possivel enviar o email com a solicitacao de cadastro. Tente novamente.");
//		}
		
		AcessoSolicitado acesso = new AcessoSolicitado();
		
		acesso.setUsuario(usuario);
		acesso.setCodigoEfetivacao(geraCodigoConfirmacaoCadastro(usuario));
		
		usuario.getHistoricoSituacaoAcesso().add(acesso);
		
		entityManager.merge(usuario);
		entityManager.flush();
	}

	private String geraCodigoConfirmacaoCadastro(Usuario usuario) throws Exception {
		Date dataAtual = Calendar.getInstance().getTime();	
		String codigo = CriptografiaUtil.criptografarMD5(usuario.getEmail() + dataAtual.toString());
		return codigo;
	}

	/**
	 * Autentica um usuario requisitando um servico da 
	 * Positivo. 
	 * 
	 * @param email do usuario
	 * @param senha do usuario
	 * @return o usuario autenticado
	 * @throws Exception caso ocorra algum problema na excecao
	 */
	public Usuario autenticarUsuario(String email, String senha) throws Exception {
		RespostaPositivo resp = autenticarNaRedePositivo(email, senha);

		Usuario usuario = recuperaUsuario(email);
		
		if(usuario == null) {
			throw new Exception("Cliente nao encontrado");
		}
		else {
			usuario.getSituacaoAcessoAtual().autenticar(resp.getCodigo() == 0);
		}
		
		switch (CodigoRespostaAutenticacao.fromCodigo(resp.getCodigo())) {
		 case SUCESSO: return recuperaUsuario(email);
		
		 case SENHA_INVALIDA: throw new Exception("Senha nao confere");
		 
		 case CLIENTE_NAO_ENCONTRADO: throw new Exception("Cliente nao encontrado"); 
		 
		 default: throw new Exception("Problemas na autenticacao do usuario");
		}
	}

	private RespostaPositivo autenticarNaRedePositivo(String email, String senha)
			throws Exception {
		AutenticacaoSRV req = new AutenticacaoSRV(email, senha);
		req.preparaRequisicao();
		RespostaPositivo resp = req.requisitaServico();
		return resp;
	}

	/*
	 * Recupera um usuario na base de dados pelo email.
	 * Nessa arquitetura, optamos por nao fazer uso do padrao
	 * Data Access Objects, dada a simplicidade do projeto. 
	 */
	private Usuario recuperaUsuario(String email) throws Exception {
		try {
			 @SuppressWarnings("unchecked")
			List<Usuario> usuarios = entityManager.createQuery(
					"FROM Usuario u where u.email = :pEmail").setParameter(
					"pEmail", email).getResultList();
			 
			 if(usuarios == null || usuarios.size() == 0) {
				 return null;
			 }
			 else return usuarios.get(0);
		}
		catch (Exception e) {
			throw new Exception("Problemas na consulta ao usuario");
		}
	}

	/**
	 * Adiciona uma proposta submetida pelo usuario logado.
	 * 
	 * @param usuarioLogado usuario logado no sistema
	 * @param proposta proposta submetida
	 */
	public void adicionarProposta(Usuario usuarioLogado, Proposta proposta) {
		if(usuarioLogado.getPropostas() == null) {
			usuarioLogado.setPropostas(new ArrayList<Proposta>());
		}
		
		usuarioLogado.getPropostas().add(proposta);
		proposta.setUsuario(usuarioLogado);

		entityManager.merge(usuarioLogado);
		entityManager.flush();
	}

	/**
	 * Utilizando contexto transacional, recupera as propostas 
	 * submetidas pelo usuario e que podem nao ter sido previamente 
	 * carregadas (uso da propriedade Lazy). 
	 * 
	 * @param usuarioLogado usuario logado no sistema
	 * @return propostas submetidas pelo usuario
	 */
	public List<Proposta> recuperaPropostas(Usuario usuarioLogado) {
		return usuarioLogado.getPropostas();
	}
}
