package br.unb.cdt.desafioPositivo.facade;

import javax.persistence.EntityManager;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;

import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.util.rest.AutenticacaoSRV;
import br.unb.cdt.desafioPositivo.util.rest.CadastroSRV;
import br.unb.cdt.desafioPositivo.util.rest.RespostaPositivo;

@Name("facade")
@AutoCreate
public class DesafioPositivoFacade {

	@In
	private EntityManager entityManager;

	/**
	 * Adiciona um usuario no meio de persistencia e realiza uma requisicao ao
	 * servico correspondente da positivo.
	 * 
	 * @param usuario
	 *            usuario a ser cadastrado
	 * @throws Exception
	 *             Caso algum problema tenha ocorrido.
	 */
	public void adicionarUsuario(Usuario usuario) throws Exception {
		CadastroSRV req = new CadastroSRV(usuario);

		req.preparaRequisicao();

		RespostaPositivo resp = req.requisitaServico();

		switch(resp.getCodigo()) {
	     case 0: entityManager.persist(usuario); break;
	     case 4: throw new Exception("Usuario jah cadastrado");
	     default: throw new Exception("Problemas na inclusao do usuario.");
		}
		throw new Exception("Nao foi possivel adicionar o usuario");
	}

	public Usuario autenticarUsuario(String email, String senha) throws Exception {
		AutenticacaoSRV req = new AutenticacaoSRV(email, senha);

		req.preparaRequisicao();

		RespostaPositivo resp = req.requisitaServico();

		switch (resp.getCodigo()) {
		 case 0:return recuperaUsuario(email);
		 case 2: throw new Exception("Senha nao confere");
		 case 3: throw new Exception("Cliente nao encontrado");
		 default : throw new Exception("Problemas na autenticacao do usuario");
		}
	}

	private Usuario recuperaUsuario(String email) throws Exception {
		try {
			return (Usuario) entityManager.createQuery(
					"FROM Usuario u where u.email = :pEmail").setParameter(
					"pEmail", email).getSingleResult();
		} catch (Exception e) {
			throw new Exception("Problemas na consulta ao usuario");
		}
	}
}
